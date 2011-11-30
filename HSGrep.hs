module HSGrep where

import Control.Monad (unless)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Maybe (isNothing, fromJust)
import System.IO (
        Handle, hTell, hSeek, hIsEOF, SeekMode (RelativeSeek), SeekMode
        (AbsoluteSeek), hFileSize)

-- Chunk of a file
data Chunk = Chunk Handle Integer Integer

-- Is char newline?
isNL :: C.ByteString -> Bool
isNL c = c == C.singleton '\n'

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
                 else do c <- C.hGet h 1
                         unless (isNL c) $
                             do hSeek h RelativeSeek (-2)
                                goToBOL h

hGetLine :: Handle -> IO C.ByteString
hGetLine h = go h C.empty
        where go :: Handle -> C.ByteString -> IO C.ByteString
              go hdl acc = do
                      c <- C.hGet hdl 1
                      if (isNL c)
                         then return $ acc
                         else go hdl (C.append acc c)

getCurrentLine :: Handle -> IO C.ByteString
getCurrentLine h = goToBOL h >> hGetLine h

getPrevLine :: Handle -> MaybeT IO C.ByteString
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

search :: Chunk -> C.ByteString -> MaybeT IO C.ByteString
search (Chunk h start end) str
        | start >= end = MaybeT $ return Nothing
        | otherwise = MaybeT $
                if mid == (end - 1) then return Nothing else do
                goTo h mid
                midLine <- liftIO $ getCurrentLine h
                prevLine <- runMaybeT $ getPrevLine h
                if str `C.isPrefixOf` midLine && (isNothing prevLine || not (str `C.isPrefixOf` fromJust prevLine))
                   then return $ Just midLine
                   else if str < midLine
                           then runMaybeT $ search (Chunk h start mid) str
                           else runMaybeT $ search (Chunk h mid end) str
           where mid = (start + end) `div` 2

hsgrep :: String -> Handle -> IO ()
hsgrep s_ h = do
        len <- hFileSize h
        _ <- runMaybeT $ search (Chunk h 0 len) s
        c <- C.hGetContents h
        C.putStrLn . C.unlines $ takeWhile (C.isPrefixOf s) (C.lines c)
        where s = C.pack s_
