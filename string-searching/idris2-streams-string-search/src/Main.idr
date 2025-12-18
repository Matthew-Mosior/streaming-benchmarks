module Main

import Data.ByteString.Search.DFA
import Data.So
import Data.String
import FS.Chunk as C
import FS.Posix
import IO.Async.Loop.Posix
import System

%default total

covering
runProg :  AsyncStream Poll [Errno] Void
        -> IO ()
runProg prog =
  simpleApp $
    mpull ( handle [stderrLn . interpolate] prog
          )

searchForATAAA :  ByteString
               -> ByteString
searchForATAAA targetbs =
  let pat   := Prelude.unpack "ATAAA"
      patbs := Data.ByteString.pack (map (cast {to=Bits8}) pat)
    in case decSo $ (not $ null patbs) of
         No  _      =>
           assert_total $ idris_crash "pat is null"
         Yes patprf =>
           case decSo $ (not $ null targetbs) of
             No  _         =>
               assert_total $ idris_crash "target is null"
             Yes targetprf =>
               let indices = ( run1 $ \t =>
                                 indicesDFA patbs targetbs {prfpat=patprf} {prftarget=targetprf} t
                             )
                 in fromString $
                      joinBy "," $
                        map show indices

search :  ChunkSize
       => String
       -> AsyncStream Poll [Errno] Void
search fp =
     readBytes fp
  |> P.mapOutput searchForATAAA
  |> writeTo Stdout

prog :  List String
     -> AsyncStream Poll [Errno] Void
prog []        =
  throw EINVAL
prog (_::size) =
  case size of
    ["small"]             =>
      search @{4096} "../resources/nucleotides_small.txt"
    ["medium"]            =>
      search @{8192} "../resources/nucleotides_medium.txt"
    ["large"]             =>
      search @{16384} "../resources/nucleotides_large.txt"
    ["extra_large"]       =>
      search @{32768} "../resources/nucleotides_extra_large.txt"
    ["extra_extra_large"] =>
      search @{65536} "../resources/nucleotides_extra_extra_large.txt"
    ["massive"]           =>
      search @{131072} "../resources/nucleotides_massive.txt"
    _                     =>
      stderrLn "Usage: pack run idris2-streams-string-search.ipkg [small|medium|large|extra_large|extra_extra_large|massive]"

covering
main : IO ()
main = getArgs >>= runProg . prog
