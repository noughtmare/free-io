{-# LANGUAGE LambdaCase #-}
module System.IO.Free where

import Data.ByteString.Lazy (ByteString)

import qualified Prelude
import Prelude (String, Bool(..), not, Maybe(..), maybe, Either(..), Functor(..), Applicative(..), Monad(..), (.), Eq(..), otherwise)
import Control.Monad (ap)
import System.IO hiding (IO)
import qualified System.IO as Handle
import System.Directory
import qualified GHC.IO
import Control.Exception (displayException)
import qualified Data.ByteString.Lazy as LBS
import qualified System.Environment as Env

data IOError
  = WriteError String
  | ReadError String
  | SearchError String
  | FormatError String
  | OtherError String

type Name = String

data IOF k
  = ReadFile Name (Either IOError String -> k)
  | WriteFile Name String (Either IOError () -> k)
  | AppendFile Name String (Either IOError () -> k)
  | ReadBinFile Name (Either IOError ByteString -> k)
  | WriteBinFile Name ByteString (Either IOError () -> k)
  | AppendBinFile Name ByteString (Either IOError () -> k)
  | DeleteFile Name (Either IOError () -> k)
  | StatusFile Name (Either IOError String -> k)
  | ReadChan Name (Either IOError String -> k)
  | AppendChan Name String (Either IOError () -> k)
  | ReadBinChan Name (Either IOError ByteString -> k)
  | AppendBinChan Name ByteString (Either IOError () -> k)
  | StatusChan Name (Either IOError String -> k)
  | Echo Bool (Either IOError () -> k)
  | GetArgs (Either IOError [String] -> k)
  | GetProgName (Either IOError String -> k)
  | GetEnv Name (Either IOError String -> k)
  | SetEnv Name String (Either IOError () -> k)
  deriving Functor

data Free f a = Pure a | Roll (f (Free f a)) deriving Functor

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance Functor f => Monad (Free f) where
  Pure x >>= k = k x
  Roll x >>= k = Roll (fmap (>>= k) x)

type IO = Free IOF

readFile :: Name -> IO (Either IOError String)
readFile n = Roll (ReadFile n pure)

writeFile :: Name -> String -> IO (Either IOError ())
writeFile n x = Roll (WriteFile n x pure)

appendFile :: Name -> String -> IO (Either IOError ())
appendFile n x = Roll (AppendFile n x pure)

readBinFile :: Name -> IO (Either IOError ByteString)
readBinFile n = Roll (ReadBinFile n pure)

writeBinFile :: Name -> ByteString -> IO (Either IOError ())
writeBinFile n x = Roll (WriteBinFile n x pure)

appendBinFile :: Name -> ByteString -> IO (Either IOError ())
appendBinFile n x = Roll (AppendBinFile n x pure)

deleteFile :: Name -> IO (Either IOError ())
deleteFile n = Roll (DeleteFile n pure)

statusFile :: Name -> IO (Either IOError String)
statusFile n = Roll (StatusFile n pure)

readChan :: Name -> IO (Either IOError String)
readChan n = Roll (ReadChan n pure)

appendChan :: Name -> String -> IO (Either IOError ())
appendChan n x = Roll (AppendChan n x pure)

readBinChan :: Name -> IO (Either IOError ByteString)
readBinChan n = Roll (ReadBinChan n pure)

appendBinChan :: Name -> ByteString -> IO (Either IOError ())
appendBinChan n x = Roll (AppendBinChan n x pure)

statusChan :: Name -> IO (Either IOError String)
statusChan n = Roll (StatusChan n pure)

echo :: Bool -> IO (Either IOError ())
echo b = Roll (Echo b pure)

getArgs :: IO (Either IOError [String])
getArgs = Roll (GetArgs pure)

getProgName :: IO (Either IOError String)
getProgName = Roll (GetProgName pure)

getEnv :: Name -> IO (Either IOError String)
getEnv n = Roll (GetEnv n pure)

setEnv :: Name -> String -> IO (Either IOError ())
setEnv n x = Roll (SetEnv n x pure)

--------------------------------------------------------------------------------
-- IO implementation
--------------------------------------------------------------------------------

failWith :: IOError -> Prelude.IO (Either IOError a)
failWith = pure . Left

data ChanMode = R | A deriving Eq

mapChan :: Name -> Maybe (Handle, ChanMode)
mapChan = \case
  "stdin"   -> Just (Handle.stdin, R)
  "stdout"  -> Just (Handle.stdout, A)
  "stderr"  -> Just (Handle.stderr, A)
  "stdecho" -> Just (Handle.stdout, A)
  _         -> Nothing


fileNotFound, chanCantRead, chanCantAppend, chanNotExist, varNotExist :: IOError
fileNotFound   = SearchError "File not found."
chanCantRead   = ReadError "Can't read from channel."
chanCantAppend = WriteError "Can't append to channel."
chanNotExist   = SearchError "Channel doesn't exist."
varNotExist    = SearchError "Environment variable doesn't exist."

ensureFileExist :: FilePath -> Prelude.IO (Either IOError a) -> Prelude.IO (Either IOError a)
ensureFileExist name io =
  doesFileExist name >>= \case
    False -> failWith fileNotFound
    True  -> io

withMode :: Name -> ChanMode -> (Handle -> Prelude.IO (Either IOError a)) -> Prelude.IO (Either IOError a)
withMode name mode f = case mapChan name of
  Nothing -> failWith chanNotExist
  Just (h, m)
    | m == mode -> f h
    | mode == R -> failWith chanCantRead
    | otherwise -> failWith chanCantAppend

catchAny :: (String -> IOError) -> Prelude.IO a -> Prelude.IO (Either IOError a)
catchAny f x = GHC.IO.catchAny (fmap Right x) (pure . Left . f . displayException)

catchAny' :: (String -> IOError) -> Prelude.IO (Either IOError a) -> Prelude.IO (Either IOError a)
catchAny' f x = GHC.IO.catchAny x (pure . Left . f . displayException)

runIO :: IO a -> Prelude.IO a
runIO (Pure x) = pure x
runIO (Roll x0) = case x0 of
  ReadFile n k -> ensureFileExist n (catchAny ReadError (Prelude.readFile n)) >>= runIO . k
  WriteFile n x k -> catchAny WriteError (Prelude.writeFile n x) >>= runIO . k
  AppendFile n x k -> ensureFileExist n (catchAny WriteError (Prelude.appendFile n x)) >>= runIO . k
  ReadBinFile n k -> ensureFileExist n (catchAny ReadError (LBS.readFile n)) >>= runIO . k
  WriteBinFile n x k -> catchAny WriteError (LBS.writeFile n x) >>= runIO . k
  AppendBinFile n x k -> ensureFileExist n (catchAny WriteError (LBS.appendFile n x)) >>= runIO . k
  DeleteFile n k -> ensureFileExist n (catchAny WriteError (removeFile n)) >>= runIO . k
  StatusFile n k -> catchAny' OtherError (do
    p <- doesPathExist n
    if not p then failWith fileNotFound else do
      f <- doesFileExist n
      d <- doesDirectoryExist n
      s <- getPermissions n
      pure (Right
        [ if f then 'f' else if d then 'd' else 'u'
        , if readable s then 'r' else '-'
        , if writable s then 'w' else '-'
        , if executable s then 'x' else '-'
        ])
      ) >>= runIO . k
  ReadChan n k -> withMode n R (\h -> catchAny ReadError (hGetContents h)) >>= runIO . k
  AppendChan n x k -> withMode n A (\h -> catchAny WriteError (hPutStr h x >> hFlush h)) >>= runIO . k
  ReadBinChan n k -> withMode n R (\h -> catchAny ReadError (LBS.hGetContents h)) >>= runIO . k
  AppendBinChan n x k -> withMode n A (\h -> catchAny WriteError (LBS.hPutStr h x >> hFlush h)) >>= runIO . k
  StatusChan n k -> case mapChan n of
    Nothing -> failWith chanNotExist >>= runIO . k
    Just _ -> runIO (k (Right "0 0"))
  Echo b k -> catchAny OtherError (hSetEcho Handle.stdin b) >>= runIO . k
  GetArgs k -> catchAny OtherError Env.getArgs >>= runIO . k
  GetProgName k -> catchAny OtherError Env.getProgName >>= runIO . k
  GetEnv n k -> catchAny' OtherError (Env.lookupEnv n >>= maybe (failWith varNotExist) (pure . Right)) >>= runIO . k
  SetEnv n x k -> catchAny OtherError (Env.setEnv n x) >>= runIO . k