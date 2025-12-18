{-# LANGUAGE OverloadedStrings #-}

import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.ByteString as PB
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as DBC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Search.DFA as DBSDFA
import System.IO (Handle, IOMode(..), withFile)
import System.Environment (getArgs)
import System.Exit (die)
import Control.Monad (forM_, unless)

processFile :: Handle -> IO ()
processFile h =
  runEffect $
    PB.fromHandle h
      >-> P.map (DBSDFA.indices "ATAAA")
      >-> P.map show
      >-> P.stdoutLn

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["small"] ->
      withFile "../resources/nucleotides_small.txt" ReadMode processFile
    ["medium"] ->
      withFile "../resources/nucleotides_medium.txt" ReadMode processFile
    ["large"] ->
      withFile "../resources/nucleotides_large.txt" ReadMode processFile
    ["extra_large"] ->
      withFile "../resources/nucleotides_extra_large.txt" ReadMode processFile
    _ ->
      die "Usage: stack exec haskell-pipes-string-search-exe [small|medium|large|extra_large]"
