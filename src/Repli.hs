{-# LANGUAGE BangPatterns, OverloadedStrings, RecordWildCards #-}
module Repli
    ( runSingle
    , Result, Err(..)
    ) where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Control.Exception (evaluate)
import Control.Monad (join)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid ((<>))
import qualified Mueval.ArgsParse as Mu
import qualified Mueval.Interpreter as Mu
import qualified Language.Haskell.Interpreter as Hint
import System.Posix.Signals (sigXCPU, installHandler, Handler(CatchOnce))

data Err
    = OptionError String
    | InterpreterError Text
    | TimeoutError
    deriving (Show)

instance ToJSON Err where
    toJSON e = object ["error" .= estr]
      where
        estr = case e of
            OptionError s -> "Repli option error: " <> T.pack s
            TimeoutError -> "Timeout"
            InterpreterError t -> t

data Result = Result
    { origExpr :: !Text
    , exprType :: !Text
    , exprValue :: !Text
    } deriving (Eq, Show)

instance ToJSON Result where
    toJSON Result{..} = object
        [ "orig_expr" .= origExpr
        , "expr_type" .= exprType
        , "expr_value" .= exprValue
        ]

runSingle :: Text -> IO (Either Err Result)
runSingle expr =
    case Mu.interpreterOpts [] of
        Left (_,msg) -> return . Left . OptionError $ msg
        Right opt -> go opt
          { Mu.namedExtensions =
            [ "RankNTypes"
            , "LiberalTypeSynonyms"
            , "RecordWildCards"
            , "BangPatterns"
            , "LambdaCase"
            , "PostfixOperators"
            , "TupleSections"
            , "RecursiveDo"
            , "MultiWayIf"
            , "ViewPatterns"
            , "PatternGuards"
            , "ScopedTypeVariables"
            , "ParallelListComp"
            , "TransformListComp"
            , "MonadComprehensions"
            ]
          , Mu.loadFile = "/tmp/Imports.hs"
          , Mu.expression = T.unpack expr
          , Mu.modules = Nothing
          }
  where
    go opt = do
        let i = Mu.interpreter opt
        fmap join $ Async.race
            timeout
            ((fmap wrapRes $ Hint.runInterpreter i) >>= evaluate)
      where
        t = T.pack
        timeout = threadDelay (10*1000000) >> return TimeoutError
        wrapRes (Left err) =
            let etext = case err of
                    Hint.UnknownError s -> T.pack s
                    Hint.NotAllowed s -> T.pack s
                    Hint.GhcException s -> T.pack s
                    Hint.WontCompile es -> T.intercalate "\n---\n"
                                         . map (T.pack . Hint.errMsg)
                                         $ es
            in Left . InterpreterError $ etext
        wrapRes (Right (a,b,c)) =
            let res = t c
            in (T.length res) `seq` Right $! Result (t a) (t b) res
