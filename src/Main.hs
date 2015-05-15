{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BL
import Data.Monoid (mconcat)
import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.Environment (getArgs)
import System.CPUTime (getCPUTime)
import qualified System.Metrics as M
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Distribution as Dist
import qualified System.Metrics.Label as Label
import System.Remote.Monitoring (forkServer, serverMetricStore)
import Web.Scotty

import Repli

serve port estore = do
    ekgSucc <- M.createCounter "repli.eval.success" estore
    ekgFail <- M.createCounter "repli.eval.fails" estore
    ekgCompTime <- M.createDistribution "repli.eval.comp_time_us" estore
    ekgLastExpr <- M.createLabel "repli.eval.last_expr" estore
    let doEval expr = do
          res <- liftIO $ timed ekgCompTime $ do
              Label.set ekgLastExpr expr
              runSingle expr
          case res of
              Left e -> counted ekgFail $ json e
              Right r -> counted ekgSucc $ json r
    scotty port $ do
        get "/eval/:expr" $ param "expr" >>= doEval
        post "/eval" $ body >>= doEval . utf
  where
    counted cnt f = liftIO (Counter.inc cnt) >> f
    timed dist f = do
        t0 <- getCPUTime
        r <- f
        t1 <- getCPUTime
        unless (t1 < t0) $  -- to avoid wraparound
            Dist.add dist (fromIntegral $ (t1 - t0) `div` 1000000)
        return r
    utf = decodeUtf8With lenientDecode . BL.toStrict
    
main = do
    (port:monitor:_) <- getArgs
    ekg <- forkServer "0.0.0.0" (read monitor)
    serve (read port) (serverMetricStore ekg)
