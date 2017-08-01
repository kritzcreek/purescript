module Language.PureScript.AST.Query where

import Protolude

import qualified Language.PureScript as P

import Control.Monad.Writer.Strict
import Data.Vector (Vector)
import Data.Vector.Algorithms.Search
import qualified Data.Vector as V
import Language.PureScript.AST

type M = WriterT (Vector (DFI Node)) (State Int)

-- Depth first index
type DFI a = (Int, Int, a)

getDFI :: DFI a -> Int
getDFI (dfi, _, _) = dfi

data Node = Declaration' Declaration | Expr' Expr | Binder' Binder

data Index = Index
  { declarations :: Vector (DFI Declaration)
  , expressions :: Vector (DFI Expr)
  , binders :: Vector (DFI Binder)
  }

emptyIndex :: Index
emptyIndex = Index V.empty V.empty V.empty

buildIndex :: Declaration -> Index
buildIndex x =
  let nodes :: Vector (DFI Node) = flip evalState 0 $ execWriterT $ goDecl x
  in foldl' insertNode emptyIndex nodes

currentIndex :: M Int
currentIndex = get

nextIndex :: M Int
nextIndex = modify succ *> get

writeNode :: DFI Node -> M ()
writeNode = tell . V.singleton

insertNode :: Index -> DFI Node -> Index
insertNode prev (i, j, node) = case node of
  Declaration' d ->
    prev { declarations = V.snoc (declarations prev) (i, j, d) }
  Expr' e ->
    prev { expressions = V.snoc (expressions prev) (i, j, e) }
  Binder' b ->
    prev { binders = V.snoc (binders prev) (i, j, b) }

goDecl :: Declaration -> M Int
goDecl d = case d of
  DataDeclaration{} ->
    writeDecl (pure 1)
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
  _ -> writeDecl (pure 1)
  where
    writeDecl g = do
      ix <- nextIndex
      descendants <- g
      writeNode (ix, descendants, Declaration' d)
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
  _ -> writeExpr (pure 1)
  where
    writeExpr g = do
      ix <- nextIndex
      descendants <- g
      writeNode (ix, descendants, Expr' expr)
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
  _ -> writeBinder (pure 1)
  where
    writeBinder g = do
      ix <- nextIndex
      descendants <- g
      writeNode (ix, descendants, Binder' b)
      pure (descendants + 1)

getDescendants :: Index -> (Int, Int) -> (Index -> Vector (DFI a)) -> Vector (DFI a)
getDescendants ix (dfi, desc) type_ = runST $ do
  let nodes = type_ ix
  let l = V.length nodes
  thawed <- V.thaw (map getDFI nodes)
  a <- binarySearchByBounds compare thawed dfi 0 l
  b <- binarySearchByBounds compare thawed (dfi + desc) (a + 1) l
  pure (V.slice a b nodes)

script = do
  let fp = "d:/Documents/GitHub/tmp/src/Main.purs"
  f <- readFile fp
  let Right (_, P.Module _ _ _ decls _) = P.parseModuleFromFile identity (fp, f)
  print (length decls)
  pure decls
