{-# LANGUAGE OverloadedStrings #-}

import Conduit
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (die)
import Control.Monad.IO.Class (liftIO)

fahrenheitToCelsius :: Double -> Double
fahrenheitToCelsius f = (f - 32) * 5 / 9

convertToCelsius :: T.Text -> Double
convertToCelsius txt =
  let f = read (T.unpack txt) :: Double
    in fahrenheitToCelsius f

processFile :: FilePath -> IO ()
processFile filePath = runConduitRes $
  sourceFile filePath
  .| decodeUtf8C
  .| linesUnboundedC
  .| mapM_C processLine
  where
    processLine :: T.Text -> ResourceT IO ()
    processLine line =
      liftIO $ do
        let tempF = T.strip line
        if T.null tempF
          then return ()
          else do
            let tempC = convertToCelsius tempF
            putStrLn $ show tempC

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["small"] ->
      processFile "../resources/fahrenheit_small.txt"
    ["medium"] ->
      processFile "../resources/fahrenheit_medium.txt"
    ["large"] ->
      processFile "../resources/fahrenheit_large.txt"
    ["extra_large"] ->
      processFile "../resources/fahrenheit_extra_large.txt"
    _ ->
      die "Usage: stack exec haskell-conduit-fahrenheit-exe [small|medium|large|extra_large]"
