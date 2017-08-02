{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveTraversable #-}
module Language.PureScript.AST.Query
  ( Index(..)
  , Indexed(..)
  , DFI(..)
  , buildIndexForModule
  , buildIndexForDeclaration
  , getDescendants
  , findCoveringDFI
  ) where

import Protolude

import qualified Language.PureScript as P

import           Control.Arrow ((&&&))
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Vector.Algorithms.Search
import Language.PureScript.AST
import           System.IO.UTF8 (readUTF8FileT)

type M = State S

data S = S
  { nodes :: Vector (Indexed Node)
  , currentIndex :: Int
  }

newtype DFI = DFI { unDFI :: Int } deriving (Show, Eq, Ord)

-- Depth first index
data Indexed a = Indexed
  { ixDFI :: DFI
  , ixDesc :: Int
  , ixVal :: a
  } deriving (Show, Eq, Ord, Functor, Foldable, Traversable)


data Node = Declaration' Declaration | Expr' Expr | Binder' Binder | Module' Module

data Index = Index
  { modules :: Vector (Indexed Module)
  , declarations :: Vector (Indexed Declaration)
  , expressions :: Vector (Indexed Expr)
  , binders :: Vector (Indexed Binder)
  , sourceSpans :: Vector (SourcePos, SourcePos)
  } deriving Show

emptyIndex :: Index
emptyIndex = Index V.empty V.empty V.empty V.empty V.empty

buildIndexForModule :: Module -> Index
buildIndexForModule m =
  let span = (spanStart &&& spanEnd) (getModuleSourceSpan m)
  in buildIndex span $ do
    ix <- nextIndex
    writeNode (ix, Module' m)
    descendants <- map sum $ traverse goDecl (getModuleDeclarations m)
    updateDescendantCount ix descendants

buildIndexForDeclarations :: [Declaration] -> Index
buildIndexForDeclarations ds =
  buildIndex undefined (traverse_ goDecl ds)

buildIndexForDeclaration :: Declaration -> Index
buildIndexForDeclaration d =
  let span = (spanStart &&& spanEnd) (declSourceSpan d)
  in buildIndex span (goDecl d)

buildIndex :: (SourcePos, SourcePos) -> M a -> Index
buildIndex span f =
  let
    result :: Vector (Indexed Node) = nodes $ flip execState (S V.empty 0) $ f
  in
    fst $ foldl' insertNode (emptyIndex, span) result

insertNode :: (Index, (SourcePos, SourcePos)) -> Indexed Node -> (Index, (SourcePos, SourcePos))
insertNode (prev, pSpan) (Indexed i j node) = case node of
  Declaration' d ->
    let
      span = (spanStart &&& spanEnd) (declSourceSpan d)
    in
      (prev
        { declarations = V.snoc (declarations prev) (Indexed i j d)
        , sourceSpans = V.snoc (sourceSpans prev) span
        }, span)
  Expr' e ->
    let
      span = maybe pSpan (spanStart &&& spanEnd) (exprSourceSpan e)
    in
      (prev
        { expressions = V.snoc (expressions prev) (Indexed i j e)
        , sourceSpans = V.snoc (sourceSpans prev) span
        }, span)
  Binder' b ->
    let
      span = maybe pSpan (spanStart &&& spanEnd) (binderSourceSpan b)
    in
      (prev
        { binders = V.snoc (binders prev) (Indexed i j b)
        , sourceSpans = V.snoc (sourceSpans prev) span
        }, span)
  Module' m ->
    let
      span = (spanStart &&& spanEnd) (getModuleSourceSpan m)
    in
      (prev
        { modules = V.snoc (modules prev) (Indexed i j m)
        , sourceSpans = V.snoc (sourceSpans prev) span
        }, span)


nextIndex :: M DFI
nextIndex = gets (DFI . currentIndex) <* modify (\s -> s {currentIndex = currentIndex s + 1})

writeNode :: (DFI, Node) -> M ()
writeNode (i, node) = modify $ \(S nodes ix) -> S (nodes `V.snoc` Indexed i 0 node) ix

updateDescendantCount :: DFI -> Int -> M ()
updateDescendantCount ix count =
  modify $ \(S nodes i) -> S ( V.modify (\v -> MV.modify v setDesc (unDFI ix)) nodes ) i
  where
   setDesc i = i { ixDesc = count }

goDecl :: Declaration -> M Int
goDecl d = case d of
  ValueDeclaration _ _ _ binders gExprs ->
    writeDecl $ do
      let exprs = map discardGuards gExprs
      bdrDesc <- for binders goBinder
      exprDesc <- for exprs goExpr
      pure (sum bdrDesc + sum exprDesc)
  BoundValueDeclaration _ binder expr ->
    writeDecl $ do
      bdrDesc <- goBinder binder
      exprDesc <- goExpr expr
      pure (bdrDesc + exprDesc)
  BindingGroupDeclaration bindings ->
    writeDecl $ map sum $ for bindings $ \(_, _, e) -> goExpr e
  TypeClassDeclaration _ _ _ _ _ decls ->
    writeDecl $ map sum $ traverse goDecl decls
  _ -> writeDecl (pure 0)
  where
    writeDecl g = do
      ix <- nextIndex
      writeNode (ix, Declaration' d)
      descendants <- g
      updateDescendantCount ix descendants
      pure (descendants + 1)

goExpr :: Expr -> M Int
goExpr expr = case expr of
  Literal lit ->
    writeExpr $ map sum $ traverse goExpr lit
  UnaryMinus e ->
    writeExpr $ goExpr e
  BinaryNoParens e1 e2 e3 ->
    writeExpr $ map sum $ traverse goExpr [e1, e2, e3]
  Parens e ->
    writeExpr $ goExpr e
  Accessor _ e ->
    writeExpr $ goExpr e
  ObjectUpdate e exps ->
    writeExpr $ do
      eDesc <- goExpr e
      eDescs <- traverse (goExpr . snd) exps
      pure (eDesc + sum eDescs)
  ObjectUpdateNested e exps ->
    writeExpr $ do
      eDesc <- goExpr e
      eDescs <- traverse goExpr exps
      pure (eDesc + sum eDescs)
  Abs b e ->
    writeExpr $ (+) <$> goBinder b <*> goExpr e
  App e1 e2 ->
    writeExpr $ (+) <$> goExpr e1 <*> goExpr e2
  IfThenElse e1 e2 e3 ->
    writeExpr $ map sum $ traverse goExpr [e1, e2, e3]
  Case exprs caseAlternatives ->
    writeExpr $ do
      exprsDesc <- traverse goExpr exprs
      cAltDescs <- for caseAlternatives $ \(CaseAlternative binders gExprs) -> do
        bindersDesc <- traverse goBinder binders
        exprsDesc' <- traverse goExpr (map discardGuards gExprs)
        pure (sum bindersDesc + sum exprsDesc')
      pure (sum exprsDesc + sum cAltDescs)
  TypedValue _ e _ ->
    writeExpr $ goExpr e
  Let decls e ->
    writeExpr $ do
      declDescs <- traverse goDecl decls
      eDesc <- goExpr e
      pure (sum declDescs + eDesc)
  Do doElements ->
    let
      goDo de = case de of
        DoNotationValue e -> goExpr e
        DoNotationBind b e -> (+) <$> goExpr e <*> goBinder b
        DoNotationLet decls -> map sum $ traverse goDecl decls
        PositionedDoNotationElement _ _ de' -> goDo de'
    in
      writeExpr $ map sum $ traverse goDo doElements
  TypeClassDictionaryConstructorApp _ e ->
     writeExpr $ goExpr e
  PositionedValue _ _ e ->
     writeExpr $ goExpr e
  _ -> writeExpr (pure 0)
  where
    writeExpr g = do
      ix <- nextIndex
      writeNode (ix, Expr' expr)
      descendants <- g
      updateDescendantCount ix descendants
      pure (descendants + 1)

goBinder :: Binder -> M Int
goBinder b = case b of
  LiteralBinder lit ->
    writeBinder $ map sum $ traverse goBinder lit
  ConstructorBinder _ binders ->
    writeBinder $ map sum $ traverse goBinder binders
  BinaryNoParensBinder b1 b2 b3 ->
    writeBinder $ map sum $ traverse goBinder [b1, b2, b3]
  ParensInBinder binder ->
    writeBinder $ goBinder binder
  NamedBinder _ binder ->
    writeBinder $ goBinder binder
  PositionedBinder _ _ binder ->
    writeBinder $ goBinder binder
  TypedBinder _ binder ->
    writeBinder $ goBinder binder
  _ -> writeBinder (pure 0)
  where
    writeBinder g = do
      ix <- nextIndex
      writeNode (ix, Binder' b)
      descendants <- g
      updateDescendantCount ix descendants
      pure (descendants + 1)

getDescendants :: Index -> (DFI, Int) -> (Index -> Vector (Indexed a)) -> Vector (Indexed a)
getDescendants ix (DFI dfi, desc) type_ = runST $ do
  let nodes = type_ ix
  if V.null nodes
    then pure V.empty
    else do
      let l = V.length nodes
      thawed <- V.thaw (map (unDFI . ixDFI) nodes)
      a <- binarySearchByBounds compare thawed dfi 0 l
      b <- binarySearchByBounds compare thawed (dfi + desc) (a + 1) l
      pure (V.slice a b nodes)

-- | Finds the narrowest Node that fully covers a given 0-indexed span.
findCoveringDFI :: Index -> (SourcePos, SourcePos) -> Maybe DFI
findCoveringDFI ix (start, end) = do
  let spans = sourceSpans ix
  let covering = V.filter covers (V.indexed spans)
  if V.null spans || V.null covering
    then Nothing
    else Just (DFI (fst (V.last covering)))
  where
    covers (_, (s, e)) = s <= start && e <= end

script :: IO Index
script = do
  let fp = "d:/Documents/Github/tmp/src/Main.purs"
  f <- readUTF8FileT fp
  let Right (_, m) = P.parseModuleFromFile identity (fp, f)
  pure (buildIndexForModule m)
  -- let (_ : decl :_) = decls
  -- let ix = buildIndex decl
  -- pure ix

