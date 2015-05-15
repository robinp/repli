module Imports where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Cont
import           Control.Monad.Error
import           Control.Monad.Fix
import           Control.Monad.Identity
import           Control.Monad.Loops
import           Control.Monad.RWS
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Aeson as A
import           Data.Array
import           Data.Bits
import           Data.Bool
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.Complex
import           Data.Dynamic
import           Data.Either
import           Data.Eq
import           Data.Fixed
import           Data.Function
import           Data.Graph
import           Data.Int
import           Data.Ix
import           Data.List
import           Data.List.Split
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Ratio
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Tree
import           Data.Tuple
import           Data.Typeable
import           Data.Word
import           Debug.SimpleReflect
import           Prelude hiding (IO,putStr,putStrLn,getLine,readLn,print,readIO,readFile,writeFile,appendFile)
import           Safe hiding (abort)
import           ShowFun
import           System.Random
import           Text.Printf

-- Convenience for golfing.
asS :: (String -> String) -> Text -> Text
asS f = T.pack . f . T.unpack

asS' = flip asS
