{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BL
import Data.Monoid (mconcat)
import Data.String (fromString)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.Environment (getArgs)
import Web.Scotty

import Repli

serve port = scotty port $ do
    get "/eval/:expr" $ param "expr" >>= doEval
    post "/eval" $ body >>= doEval . utf
  where
    utf = decodeUtf8With lenientDecode . BL.toStrict
    doEval expr = do
        res <- liftIO $ runSingle expr
        case res of
            Left e -> json e
            Right r -> json r

main = do
    (port:_) <- getArgs
    serve (read port)
