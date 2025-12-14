{-# LANGUAGE OverloadedStrings #-}

import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import System.IO (Handle, IOMode(..), withFile)
import System.Environment (getArgs)
import System.Exit (die)
import Control.Monad (forM_, unless)

fahrenheitToCelsius :: Double -> Double
fahrenheitToCelsius f = (f - 32) * 5 / 9

processLine :: T.Text -> IO ()
processLine line = do
  let tempF = T.strip line
  unless (T.null tempF) $
    case reads (T.unpack tempF) :: [(Double, String)] of
      [(f, "")] -> print (fahrenheitToCelsius f)
      _         -> putStrLn $ "Invalid input: " ++ T.unpack line

processFile :: Handle -> IO ()
processFile h = do
  contents <- PB.toLazyM (PB.fromHandle h)
  let decoded = TE.decodeUtf8 (BL.toStrict contents)
  forM_ (T.lines decoded) processLine

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["small"] ->
      withFile "../resources/fahrenheit_small.txt" ReadMode processFile
    ["medium"] ->
      withFile "../resources/fahrenheit_medium.txt" ReadMode processFile
    ["large"] ->
      withFile "../resources/fahrenheit_large.txt" ReadMode processFile
    ["extra_large"] ->
      withFile "../resources/fahrenheit_extra_large.txt" ReadMode processFile
    _ ->
      die "Usage: stack exec haskell-pipes-fahrenheit-exe [small|medium|large|extra_large]"
