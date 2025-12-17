{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conduit
import qualified Data.Text as T
import qualified Data.Conduit.Network as CN
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)
import System.Exit (die)
import qualified Data.ByteString.Char8 as B8

fahrenheitToCelsius :: Double -> Double
fahrenheitToCelsius f = (f - 32) * 5 / 9

processLine :: T.Text -> IO ()
processLine line =
  let txt = T.strip line
  in if T.null txt
        then pure ()
        else case reads (T.unpack txt) :: [(Double, String)] of
               [(f, "")] ->
                 let c = fahrenheitToCelsius f
                 in putStrLn $ "Fahrenheit: " ++ show f ++ ", Celsius: " ++ show c
               _ ->
                 putStrLn $ "Invalid input: " ++ T.unpack txt

main :: IO ()
main = do
  args <- getArgs
  case args of
    [portStr] -> do
      let port = read portStr
          settings = CN.clientSettings port (B8.pack "localhost")
      CN.runTCPClient settings $ \appData ->
        runConduitRes $
          CN.appSource appData
            .| decodeUtf8C
            .| linesUnboundedC
            .| mapM_C (liftIO . processLine)
    _ ->
      die "Usage: stack exec haskell-conduit-fahrenheit-exe <port>"
