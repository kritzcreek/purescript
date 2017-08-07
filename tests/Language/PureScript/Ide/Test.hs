{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports    #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
module Language.PureScript.Ide.Test where

import Control.Category ((>>>))
import           Control.Concurrent.STM
import           "monad-logger" Control.Monad.Logger
import           Control.Monad.Writer
import qualified Data.Map                        as Map
import qualified Data.Vector as V
import           Language.PureScript.Ide
import           Language.PureScript.Ide.Command
import           Language.PureScript.Ide.Error
import           Language.PureScript.Ide.Rebuild (sortExterns)
import           Language.PureScript.Ide.Types
import           Language.PureScript.AST.Index
import Language.PureScript.Sugar.Names (desugarImports)
import           Protolude
import           System.Directory
import           System.FilePath
import           System.IO.UTF8
import           System.Process
import           Text.Show

import qualified Language.PureScript             as P

defConfig :: IdeConfiguration
defConfig =
  IdeConfiguration
    { confLogLevel = LogNone
    , confOutputPath = "output/"
    , confGlobs = ["src/*.purs"]
    , confEditorMode = False
    }

runIde' :: IdeConfiguration -> IdeState -> [Command] -> IO ([Either IdeError Success], IdeState)
runIde' conf s cs = do
  stateVar <- newTVarIO s
  let env' = IdeEnvironment {ideStateVar = stateVar, ideConfiguration = conf}
  r <- runNoLoggingT (runReaderT (traverse (runExceptT . handleCommand) cs) env')
  newState <- readTVarIO stateVar
  pure (r, newState)

runIde :: [Command] -> IO ([Either IdeError Success], IdeState)
runIde = runIde' defConfig emptyIdeState

runIde'' s f = do
  stateVar <- newTVarIO s
  let env' = IdeEnvironment {ideStateVar = stateVar, ideConfiguration = defConfig}
  Right r <- runNoLoggingT (runReaderT (runExceptT f) env')
  newState <- readTVarIO stateVar
  pure (r, newState)

volatileState :: IdeState -> [(Text, [IdeDeclarationAnn])] -> IdeState
volatileState s ds =
  s {ideVolatileState = vs}
  where
    vs = IdeVolatileState (AstData Map.empty) (Map.fromList decls) Nothing
    decls = map (first P.moduleNameFromString) ds

-- | Adding Annotations to IdeDeclarations
ann :: IdeDeclarationAnn -> Annotation -> IdeDeclarationAnn
ann (IdeDeclarationAnn _ d) a = IdeDeclarationAnn a d

annLoc :: IdeDeclarationAnn -> P.SourceSpan -> IdeDeclarationAnn
annLoc (IdeDeclarationAnn a d) loc = IdeDeclarationAnn a {_annLocation = Just loc} d

annExp :: IdeDeclarationAnn -> Text -> IdeDeclarationAnn
annExp (IdeDeclarationAnn a d) e = IdeDeclarationAnn a {_annExportedFrom = Just (mn e)} d

annTyp :: IdeDeclarationAnn -> P.Type -> IdeDeclarationAnn
annTyp (IdeDeclarationAnn a d) ta = IdeDeclarationAnn a {_annTypeAnnotation = Just ta} d


ida :: IdeDeclaration -> IdeDeclarationAnn
ida = IdeDeclarationAnn emptyAnn

-- | Builders for Ide declarations
ideValue :: Text -> Maybe P.Type -> IdeDeclarationAnn
ideValue i ty = ida (IdeDeclValue (IdeValue (P.Ident i) (fromMaybe P.tyString ty)))

ideType :: Text -> Maybe P.Kind -> [(P.ProperName 'P.ConstructorName, P.Type)] -> IdeDeclarationAnn
ideType pn ki dtors = ida (IdeDeclType (IdeType (P.ProperName pn) (fromMaybe P.kindType ki) dtors))

ideSynonym :: Text -> Maybe P.Type -> Maybe P.Kind -> IdeDeclarationAnn
ideSynonym pn ty kind = ida (IdeDeclTypeSynonym (IdeTypeSynonym (P.ProperName pn) (fromMaybe P.tyString ty) (fromMaybe P.kindType kind)))

ideTypeClass :: Text -> P.Kind -> [IdeInstance] -> IdeDeclarationAnn
ideTypeClass pn kind instances = ida (IdeDeclTypeClass (IdeTypeClass (P.ProperName pn) kind instances))

ideDtor :: Text -> Text -> Maybe P.Type -> IdeDeclarationAnn
ideDtor pn tn ty = ida (IdeDeclDataConstructor (IdeDataConstructor (P.ProperName pn) (P.ProperName tn) (fromMaybe P.tyString ty)))

ideValueOp :: Text -> P.Qualified (Either Text Text) -> Integer -> Maybe P.Associativity -> Maybe P.Type -> IdeDeclarationAnn
ideValueOp opName ident precedence assoc t =
  ida (IdeDeclValueOperator
       (IdeValueOperator
        (P.OpName opName)
        (bimap P.Ident P.ProperName <$> ident)
        (precedence)
        (fromMaybe P.Infix assoc)
        t))

ideTypeOp :: Text -> P.Qualified Text -> Integer -> Maybe P.Associativity -> Maybe P.Kind -> IdeDeclarationAnn
ideTypeOp opName ident precedence assoc k =
  ida (IdeDeclTypeOperator
       (IdeTypeOperator
        (P.OpName opName)
        (P.ProperName <$> ident)
        (precedence)
        (fromMaybe P.Infix assoc)
        k))

ideKind :: Text -> IdeDeclarationAnn
ideKind pn = ida (IdeDeclKind (P.ProperName pn))

valueSS, synonymSS, typeSS, classSS, valueOpSS, typeOpSS :: P.SourceSpan
valueSS = ss 3 1
synonymSS = ss 5 1
typeSS = ss 7 1
classSS = ss 8 1
valueOpSS = ss 12 1
typeOpSS = ss 13 1

ss :: Int -> Int -> P.SourceSpan
ss x y = P.SourceSpan "Test.purs" (P.SourcePos x y) (P.SourcePos x y)

mn :: Text -> P.ModuleName
mn = P.moduleNameFromString

inProject :: IO a -> IO a
inProject f = do
  cwd' <- getCurrentDirectory
  setCurrentDirectory ("." </> "tests" </> "support" </> "pscide")
  a <- f
  setCurrentDirectory cwd'
  pure a

compileTestProject :: IO Bool
compileTestProject = inProject $ do
  (_, _, _, procHandle) <-
    createProcess $ (shell $ "purs compile \"src/**/*.purs\"")
  r <- tryNTimes 10 (getProcessExitCode procHandle)
  pure (fromMaybe False (isSuccess <$> r))

isSuccess :: ExitCode -> Bool
isSuccess ExitSuccess = True
isSuccess (ExitFailure _) = False

tryNTimes :: Int -> IO (Maybe a) -> IO (Maybe a)
tryNTimes 0 _ = pure Nothing
tryNTimes n action = do
  r <- action
  case r of
    Nothing -> do
      threadDelay 500000
      tryNTimes (n - 1) action
    Just a -> pure (Just a)

deleteOutputFolder :: IO ()
deleteOutputFolder = inProject $
  whenM (doesDirectoryExist "output") (removeDirectoryRecursive "output")

evalWriterT :: Functor m => WriterT w m a -> m a
evalWriterT = map fst . runWriterT


script x y = inProject $ do
  let myFilterPos = P.SourcePos x y
  (_, state) <- runIde [LoadSync []]
  let path = "src/Desugar.purs"
  file <- readUTF8FileT path
  let Right (_, m) = P.parseModuleFromFile identity (path, file)
  let myModule = P.importPrim m
  (sorted, _) <- runIde'' state (state & ideFileState & fsExterns & sortExterns myModule)
  Right [desugared] <- evalWriterT $ runExceptT $ P.evalSupplyT 0 $ miniDesugar sorted [myModule]
  let ix = buildIndexForModule desugared
  let Just dfi = findCoveringDFI ix (myFilterPos, myFilterPos)
  -- traceShowM (expressions ix)

  traceShowM dfi
  traceShowM $ expressions ix & V.find ((==) dfi . ixDFI)
  traceShowM $ binders ix & V.find ((==) dfi . ixDFI)
  traceShowM $ declarations ix & V.find ((==) dfi . ixDFI)
  traceM $ case expressions ix & V.find ((==) dfi . ixDFI) & map ixVal of
    Just (P.Var (P.Qualified (Just mn) _)) -> Protolude.show mn
    Just (P.Constructor (P.Qualified (Just mn) _)) -> Protolude.show mn
  _ -> "lol"
  traceM $ case binders ix & V.find ((==) dfi . ixDFI) & map ixVal of
    Just (P.ConstructorBinder (P.Qualified (Just mn) _) _) -> Protolude.show mn
    _ -> "lol"
  pure ix

miniDesugar externs =
  map P.desugarSignedLiterals
    >>> traverse P.desugarObjectConstructors
    >=> traverse P.desugarDoModule
    >=> map P.desugarLetPatternModule
    >>> traverse P.desugarCasesModule
    >=> traverse P.desugarTypeDeclarationsModule
    >=> desugarImports externs
