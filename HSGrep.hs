module HSGrep where

import Control.Monad (unless)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Data.List (isPrefixOf)
import Data.Maybe (isNothing, fromJust)
import System.IO (
        Handle, hTell, hSeek, hIsEOF, SeekMode (RelativeSeek),
        SeekMode (AbsoluteSeek), hGetChar, hGetLine, hGetContents,
        hFileSize)

-- Chunk of a file
data Chunk = Chunk Handle Integer Integer

-- Is char newline?
isNL :: Char -> Bool
isNL c = c == '\n'

-- Are we at the beginning of file?
isBOF :: Handle -> IO Bool
isBOF = fmap (== 0) . hTell

-- Go to beginning of line
goToBOL :: Handle -> IO ()
goToBOL h = do
        bof <- isBOF h
        unless bof $
           do eof <- hIsEOF h
              if eof
                 then do hSeek h RelativeSeek (-2)
                         goToBOL h
                 else do c <- hGetChar h
                         unless (isNL c) $
                             do hSeek h RelativeSeek (-2)
                                goToBOL h

getCurrentLine :: Handle -> IO String
getCurrentLine h = goToBOL h >> hGetLine h

getPrevLine :: Handle -> MaybeT IO String
getPrevLine h = MaybeT $
        goToBOLAndDo h $ do
                hSeek h RelativeSeek (-2)
                goToBOLAndDo h $ do
                        hSeek h RelativeSeek (-2)
                        goToBOL h
                        line <- hGetLine h
                        return $ Just line
        where goToBOLAndDo h' f = do
                goToBOL h'
                bof <- isBOF h'
                if bof
                   then return Nothing
                   else f

goTo :: Handle -> Integer -> IO ()
goTo h = hSeek h AbsoluteSeek

search :: Chunk -> String -> MaybeT IO String
search (Chunk h start end) str
        | start >= end = MaybeT $ return Nothing
        | otherwise = MaybeT $
                if mid == (end - 1) then return Nothing else do
                goTo h mid
                midLine <- liftIO $ getCurrentLine h
                prevLine <- runMaybeT $ getPrevLine h
                if str `isPrefixOf` midLine && (isNothing prevLine || not (str `isPrefixOf` fromJust prevLine))
                   then return $ Just midLine
                   else if str < midLine
                           then runMaybeT $ search (Chunk h start mid) str
                           else runMaybeT $ search (Chunk h mid end) str
           where mid = (start + end) `div` 2

hsgrep :: String -> Handle -> IO ()
hsgrep s h = do
        len <- hFileSize h
        _ <- runMaybeT $ search (Chunk h 0 len) s
        --  putStrLn $ show match
        c <- hGetContents h
        putStrLn . unlines $ takeWhile (isPrefixOf s) (lines c)
