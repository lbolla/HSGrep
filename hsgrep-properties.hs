module Main where

import Control.Exception (finally)
import qualified Data.ByteString.Char8 as C
import HSGrep
import Prelude hiding (catch)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (Handle, openTempFile, hClose)
import System.IO.Error (catch)
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

prop_isNL :: Char -> Bool
prop_isNL '\n' = isNL $ C.pack "\n"
prop_isNL c = not . isNL $ C.pack [c]

--  prop_isBOF_newFile :: IO Bool
--  prop_isBOF_newFile = withTempFile "prop_isBOF" $ \f h ->
--          isBOF h

tests :: [Test]
tests = [
        testGroup "isNL" [
                testProperty "isNL" prop_isNL
                ]--,
        --  testGroup "isBOF" [
        --          testProperty "newFile" prop_isBOF_newFile
        --          ]
        ]

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func = do
        tempdir <- catch getTemporaryDirectory (\_ -> return ".")
        (tempfile, temph) <- openTempFile tempdir pattern
        finally (func tempfile temph)
                (do hClose temph
                    removeFile tempfile)

main :: IO ()
main = defaultMain tests
