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
import qualified Mueval.ArgsParse as Mu
import qualified Mueval.Interpreter as Mu
import qualified Language.Haskell.Interpreter as Hint
import System.Posix.Signals (sigXCPU, installHandler, Handler(CatchOnce))

data Err
    = OptionError String
    | InterpreterError Hint.InterpreterError
    | TimeoutError
    deriving (Show)

instance ToJSON Err where
    toJSON e = object ["error" .= T.pack (show e)]

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
          }
  where
    go opt = do
        let i = Mu.interpreter opt
        fmap join $ Async.race
            (d "timeout" timeout)
            (d "runI" $ (fmap wrapRes $ Hint.runInterpreter i) >>= evaluate)
      where
        d _ = id
        t = T.pack
        timeout = threadDelay (10*1000000) >> return TimeoutError
        wrapRes (Left err) = Left . InterpreterError $ err
        wrapRes (Right (a,b,c)) =
            let res = t c
            in (T.length res) `seq` Right $! Result (t a) (t b) res
