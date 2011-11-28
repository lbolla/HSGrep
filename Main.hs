import HSGrep
import System.Environment (getArgs)
import System.IO (withFile, IOMode (ReadMode))

main :: IO ()
main = do
        (s:fname:_) <- getArgs
        withFile fname ReadMode (hsgrep s)
