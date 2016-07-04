

import GHC.IO.Encoding (setForeignEncoding)
import Data.Maybe
import Data.List -- (intercalate, intersperse, concat)
import Data.MIME.Types
import Data.Version
import Data.Time.Clock
import Data.IORef
import Data.Char
import qualified Data.CaseInsensitive as CI
import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as LazyEnc
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as C
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.UTF8       as BU
import Data.Text.Encoding

import System.FilePath
import System.IO
import System.Console.GetOpt
import System.Posix.Process
import System.Directory
import System.Exit
import Control.Applicative hiding (optional)
import Control.Monad
import Control.Concurrent
import Control.Arrow hiding (app)
import Control.Exception (throw, throwIO)

import Network.Wai as Wai
import Network.Wai.Handler.Warp
-- import Network.Wai.Handler.WebSockets
-- import Network.WebSockets as WS
import Network.HTTP.Types
import Blaze.ByteString.Builder as Blaze (fromLazyByteString)

import Data.Attoparsec.ByteString as Atto
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text

import Database.ZDBC

import Parser

main :: IO ()
main = do 
  args <- getArgs 
  unless (null args) (putStrLn "Use DSS file as argument" >> exitSuccess)
  dssText <- readFile (head $ args)
  putStrLn $ show $ parseDSS dssText


