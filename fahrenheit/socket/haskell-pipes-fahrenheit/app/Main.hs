{-# LANGUAGE OverloadedStrings #-}

module Main where

import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import System.Environment (getArgs)
import System.Exit (die)
import Control.Monad (unless, forever)
import Control.Monad.IO.Class (liftIO)

-- Convert Fahrenheit to Celsius
fahrenheitToCelsius :: Double -> Double
fahrenheitToCelsius f = (f - 32) * 5 / 9

-- Process a single line of text
processLine :: T.Text -> IO ()
processLine line = do
    let txt = T.strip line
    unless (T.null txt) $
        case reads (T.unpack txt) :: [(Double, String)] of
            [(f, "")] ->
                let c = fahrenheitToCelsius f
                in putStrLn $ "Fahrenheit: " ++ show f ++ ", Celsius: " ++ show c
            _ ->
                putStrLn $ "Invalid input: " ++ T.unpack txt

-- Producer that reads bytes from socket and yields lines as Text
socketProducer :: NS.Socket -> Producer T.Text IO ()
socketProducer sock = go BS.empty
  where
    go leftover = do
      chunk <- liftIO $ NSB.recv sock 4096
      if BS.null chunk
        then do
          -- End of stream: yield any remaining text
          unless (BS.null leftover) $ yield (TE.decodeUtf8 leftover)
        else do
          let combined = leftover <> chunk
              -- Split on newline
              (linesBS, rest) = splitLines combined
          mapM_ (yield . TE.decodeUtf8) linesBS
          go rest

-- Split a ByteString into lines (without losing leftover partial line)
splitLines :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitLines bs =
  let ls = BS.split 10 bs  -- 10 = newline '\n'
      (fullLines, rest) = if BS.last bs == 10
                           then (ls, BS.empty)
                           else (init ls, last ls)
  in (fullLines, rest)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [portStr] -> do
      addr:_ <- NS.getAddrInfo Nothing (Just "localhost") (Just portStr)
      sock <- NS.socket (NS.addrFamily addr) NS.Stream NS.defaultProtocol
      NS.connect sock (NS.addrAddress addr)
      runEffect $ socketProducer sock >-> P.mapM_ (liftIO . processLine)
    _ ->
      die "Usage: haskell-pipes-fahrenheit-exe <port>"
