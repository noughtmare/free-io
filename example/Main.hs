import Prelude hiding (IO)
import qualified Prelude
import System.IO.Free

main :: Prelude.IO ()
main = runIO (() <$ appendChan "stdout" "Hello, World!")