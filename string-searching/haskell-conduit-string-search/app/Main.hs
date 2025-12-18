{-# LANGUAGE OverloadedStrings #-}

import Conduit
import qualified Data.ByteString.Char8 as DBC
import qualified Data.ByteString.Search.DFA as DBSDFA
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (die)
import Control.Monad.IO.Class (liftIO)

processFile :: FilePath -> IO ()
processFile filePath = runConduitRes $
  sourceFile filePath
    .| mapM_C processChunk
  where
    processChunk :: DBC.ByteString -> ResourceT IO ()
    processChunk chunk =
      liftIO $ do
        let indices = DBSDFA.indices (DBC.pack "ATAAA") chunk
        putStrLn $ L.intercalate "," $ map show indices

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["small"] ->
      processFile "../resources/nucleotides_small.txt"
    ["medium"] ->
      processFile "../resources/nucleotides_medium.txt"
    ["large"] ->
      processFile "../resources/nucleotides_large.txt"
    ["extra_large"] ->
      processFile "../resources/nucleotides_extra_large.txt"
    _ ->
      die "Usage: stack exec haskell-conduit-string-search-exe [small|medium|large|extra_large]"
