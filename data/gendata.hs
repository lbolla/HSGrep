import System.Environment (getArgs)

main :: IO ()
main = do
        (n:_) <- getArgs
        sequence_ $ map (putStrLn . show) $ take ((read n) :: Int) [1000000000..]
