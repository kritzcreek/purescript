{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Language.PureScript.Parser.LexerSpec where

import Protolude
import qualified Language.PureScript.Parser.Lexer as Lexer
import qualified Text.Parsec as Parsec
import Test.Hspec

import Data.Text (unlines)

singleToken' :: ((Int, Int), (Int, Int), Lexer.Token) -> Lexer.PositionedToken -> Bool
singleToken' ((startLine, startColumn), (endLine, endColumn), token) Lexer.PositionedToken{..} =
  Parsec.sourceLine ptSourcePos == startLine
  && Parsec.sourceLine ptEndPos == endLine
  && Parsec.sourceColumn ptSourcePos == startColumn
  && Parsec.sourceColumn ptEndPos == endColumn
  && ptToken == token
singleToken' _ _ = error "Failed parse"

singleToken
  :: (Int, Int)
  -> (Int, Int)
  -> Lexer.Token
  -> Either t [Lexer.PositionedToken] -> Bool
singleToken a b c (Right [pt]) = singleToken' (a, b, c) pt

multipleTokens
  :: [((Int, Int), (Int, Int), Lexer.Token)]
  -> Either t [Lexer.PositionedToken] -> Bool
multipleTokens tkns (Right pts) = all identity (zipWith singleToken' tkns pts)

spec :: Spec
spec = do
  describe "lexing single tokens" $ do
    it "lexes LNames" $ do
      -- I expect (1, 1) (1, 6) here.
      Lexer.lex "" "module" `shouldSatisfy` singleToken (1, 1) (1, 7) (Lexer.LName "module")
      Lexer.lex "" "module'" `shouldSatisfy` singleToken (1, 1) (1, 8) (Lexer.LName "module'")
    it "lexes UNames" $ do
      Lexer.lex "" "Module" `shouldSatisfy` singleToken (1, 1) (1, 7) (Lexer.UName "Module")
    it "lexes qualifiers" $ do
      --                                                                     | I'd expect this to be a 7
      --                                                                     v
      Lexer.lex "" "Module.Hi" `shouldSatisfy` multipleTokens [ ((1, 1), (1, 8), (Lexer.Qualifier "Module"))
                                                              , ((1, 8), (1, 10), (Lexer.UName "Hi"))
                                                              ]
  describe "lexing import statements" $ do
    it "should lex an implicit import" $ do
      Lexer.lex "" "import My.Prelude"
        `shouldSatisfy` multipleTokens
          [ ((1, 1), (1, 7), (Lexer.LName "import"))
          , ((1, 8), (1, 11), (Lexer.Qualifier "My"))
          , ((1, 11), (1, 18), (Lexer.UName "Prelude"))
          ]
    it "should lex a qualified import" $ do
      Lexer.lex "" "import Data.String as String"
        `shouldSatisfy` multipleTokens
          [ ((1, 1), (1, 7), (Lexer.LName "import"))
          -- I think it counts the dot for the length of the Data qualifier that
          -- still leaves the question of why it starts one character after the
          -- import which clearly omits the whitespace
          , ((1, 8), (1, 13), (Lexer.Qualifier "Data"))
          -- Why is this not a Qualifier?
          --
          -- Expectation: It's not followed by a dot
          , ((1, 13), (1, 19), (Lexer.UName "String"))
          , ((1, 20), (1, 22), (Lexer.LName "as"))
          , ((1, 23), (1, 29), (Lexer.UName "String"))
          ]
    it "should lex an explicit import" $ do
      Lexer.lex "" "import Control.Monad.Aff.AVar (AVAR)"
        `shouldSatisfy` multipleTokens
          [ ((1, 1), (1, 7), (Lexer.LName "import"))
          , ((1, 8), (1, 16), (Lexer.Qualifier "Control"))
          , ((1, 16), (1, 22), (Lexer.Qualifier "Monad"))
          , ((1, 22), (1, 26), (Lexer.Qualifier "Aff"))
          , ((1, 26), (1, 30), (Lexer.UName "AVar"))
          , ((1, 31), (1, 32), Lexer.LParen)
          , ((1, 32), (1, 36), (Lexer.UName "AVAR"))
          , ((1, 36), (1, 37), Lexer.RParen)
          ]

file :: Text
file = unlines
  [ "module Main where"
  , "-- (setq psc-ide-debug t)"
  , ""
  , "import Prelude"
  , "import Control.Monad.Reader.Class as Reader"
  , "import Data.String as String"
  , "import Node.Process as Process"
  , "import Control.Monad.Aff (runAff, later', attempt)"
  , "import Control.Monad.Aff.AVar (AVAR)"
  , "import Control.Monad.Eff (Eff)"
  , "import Control.Monad.Eff.Class (class MonadEff, liftEff)"
  , "import Control.Monad.Eff.Console (log, CONSOLE)"
  , "import Control.Monad.Eff.Exception (EXCEPTION)"
  , "import Control.Monad.Eff.Random (RANDOM)"
  , "import Control.Monad.Eff.Ref (writeRef, readRef, Ref, newRef, REF)"
  , "import Control.Monad.Eff.Unsafe (unsafeCoerceEff)"
  , "import Control.Monad.Reader (class MonadAsk)"
  , "import Control.Monad.Reader.Trans (ReaderT, runReaderT)"
  , "import Control.Monad.ST (runST)"
  , "import Data.Argonaut (Json)"
  , "import Data.Array (concatMap, head, null)"
  , "import Data.Either (Either(..), either, isRight)"
  , "import Data.Function.Eff (runEffFn2, EffFn2)"
  , "import Data.Maybe (Maybe(..))"
  , "import Data.Newtype (unwrap, wrap)"
  , "import Data.String (Pattern(..))"
  , "import Node.ChildProcess (CHILD_PROCESS)"
  , "import Node.FS (FS)"
  , "import PscIde (sendCommandR, load, cwd, NET)"
  , "import PscIde.Command (Command(RebuildCmd), Message(Message))"
  , "import Pscid.Console (owl, clearConsole, suggestionHint, startScreen)"
  , "import Pscid.Error (catchLog, noSourceDirectoryError)"
  , "import Pscid.Keypress (Key(..), initializeKeypresses, onKeypress)"
  , "import Pscid.Options (PscidSettings, optionParser)"
  , "import Pscid.Process (execCommand)"
  , "import Pscid.Psa (filterWarnings, PsaError, parseErrors, psaPrinter)"
  , "import Pscid.Server (restartServer, startServer', stopServer')"
  , "import Pscid.Util (launchAffVoid, both, (∘))"
  , "import Suggest (applySuggestions)"
  , "type PscidEffects = PscidEffects' ()"
  , "type PscidEffects' e ="
  , "( cp ∷ CHILD_PROCESS"
  , ", console ∷ CONSOLE"
  , ", net ∷ NET"
  , ", avar ∷ AVAR"
  , ", fs ∷ FS"
  , ", process ∷ Process.PROCESS"
  , ", random ∷ RANDOM"
  , ", ref ∷ REF"
  , "| e"
  , ")"
  , "newtype Pscid a = Pscid (ReaderT (PscidSettings Int) (Eff PscidEffects) a)"
  , "derive newtype instance functorPscid ∷ Functor Pscid"
  , "derive newtype instance bindPscid ∷ Bind Pscid"
  , "derive newtype instance monadPscid ∷ Monad Pscid"
  , "derive newtype instance monadAskPscid ∷ MonadAsk (PscidSettings Int) Pscid"
  , "instance monadEffPscid ∷ MonadEff e Pscid where"
  , "liftEff f = Pscid (liftEff (unsafeCoerceEff f))"
  , ""
  , "runPscid ∷ ∀ a. Pscid a → PscidSettings Int → Eff PscidEffects a"
  , "runPscid (Pscid f) e = runReaderT f e"
  , ""
  , "newtype State = State { errors ∷ Array PsaError }"
  , ""
  , "emptyState ∷ State"
  , "emptyState = State { errors: [] }"
  ]
