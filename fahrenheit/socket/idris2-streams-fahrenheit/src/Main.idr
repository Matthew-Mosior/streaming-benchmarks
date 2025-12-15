module Main

import Data.ByteString
import Data.SortedMap
import Data.Vect
import Derive.Prelude
import FS.Posix
import FS.Socket
import IO.Async.Loop.Epoll
import IO.Async.Loop.Posix
import System

%default total

%language ElabReflection

addr :  Bits16
     -> IP4Addr
addr = IP4 [127,0,0,1]

data FahrenheitAndCelsiusErr : Type where
  InvalidFahrenheitValue : FahrenheitAndCelsiusErr
  OverallStreamError : FahrenheitAndCelsiusErr

%runElab derive "FahrenheitAndCelsiusErr" [Show,Eq,Ord]

ToBuf FahrenheitAndCelsiusErr where
  unsafeToBuf err = Right $ (fromString . show) err

0 FahrenheitAndCelsiusPull : Type -> Type -> Type
FahrenheitAndCelsiusPull o r = AsyncPull Poll o [Errno,FahrenheitAndCelsiusErr] r

0 FahrenheitAndCelsiusStream : Type -> Type
FahrenheitAndCelsiusStream o = AsyncPull Poll o [Errno,FahrenheitAndCelsiusErr] ()

record FahrenheitAndCelsius where
  constructor FAndC
  fahrenheitvalue : ByteString
  celsiusvalue : ByteString

convertFahrenheitToCelsius :  FahrenheitAndCelsiusPull (List ByteString) (FahrenheitAndCelsiusStream ByteString)
                           -> FahrenheitAndCelsiusPull o (Maybe ByteString, FahrenheitAndCelsiusStream ByteString)
convertFahrenheitToCelsius fahrenheitvalues = Prelude.do
  (fahrenheitandcelsius, fahrenheitstream)
    <- foldPairE toFAndC [] fahrenheitvalues
  pure $ (Just $ unixUnlines $ map createFinalBS fahrenheitandcelsius, fahrenheitstream)
  where
    toFAndC :  List FahrenheitAndCelsius
            -> List ByteString
            -> Either FahrenheitAndCelsiusErr (List FahrenheitAndCelsius)
    toFAndC fandc []     =
      Right fandc
    toFAndC fandc (h::t) =
      case Data.ByteString.parseDouble h of
        Nothing =>
          Left InvalidFahrenheitValue
        Just f' =>
          let c = (f' - 32.0) * (5.0 / 9.0)
            in toFAndC (fandc ++ [FAndC (fromString $ show f') (fromString $ show c)]) t
    createFinalBS :  FahrenheitAndCelsius
                  -> ByteString
    createFinalBS (FAndC f c) =
      append (append (fromString "Fahrenheit: ") f) (append (fromString ", Celsius: ") c)

toCelsius :  FahrenheitAndCelsiusStream ByteString
          -> FahrenheitAndCelsiusPull o (Maybe ByteString, FahrenheitAndCelsiusStream ByteString)
toCelsius fs =
     breakAtSubstring pure "\r\n\r\n" fs
  |> lines
  |> convertFahrenheitToCelsius

outputFahrenheitAndCelsiusBS :  (Maybe ByteString, FahrenheitAndCelsiusStream ByteString)
                             -> FahrenheitAndCelsiusStream ByteString
outputFahrenheitAndCelsiusBS (Nothing, _)   =
  pure ()
outputFahrenheitAndCelsiusBS (Just fandcbs, fahrenheitstream) =
  cons fandcbs fahrenheitstream 

echo :  FahrenheitAndCelsiusPull ByteString (Maybe ByteString, FahrenheitAndCelsiusStream ByteString)
     -> AsyncStream Poll [Errno] Void
echo p =
  extractErr FahrenheitAndCelsiusErr (writeTo Stdout (p >>= outputFahrenheitAndCelsiusBS)) >>= \case
    Left _   =>
         emit OverallStreamError
      |> writeTo Stdout
    Right () =>
      pure ()

covering
serve :  Socket AF_INET
      -> Async Poll [] ()
serve cli =
  flip guarantee (close' cli) $
    mpull $ handleErrors (\(Here x) => stderrLn "\{x}") $
         bytes cli 0xfff
      |> toCelsius
      |> echo

covering
echoSrv :  Bits16
        -> AsyncStream Poll [Errno] Void
echoSrv port =
  foreachPar 1
             serve (acceptOn AF_INET SOCK_STREAM (addr port))

covering
prog :  List String
     -> AsyncStream Poll [Errno] Void
prog []     =
  throw EINVAL
prog [port] =
  echoSrv ( cast {to=Bits16} port
          )
prog _      =
  stderrLn "Usage: pack run idris2-streams-fahrenheit.ipkg [port]"

covering
runProg :  AsyncStream Poll [Errno] Void
        -> IO ()
runProg prog =
  epollApp $
    mpull ( handle [stderrLn . interpolate] prog
          )

covering
main : IO ()
main = getArgs >>= runProg . prog
