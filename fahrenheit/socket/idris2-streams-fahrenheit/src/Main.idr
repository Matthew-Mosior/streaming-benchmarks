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

%runElab derive "FahrenheitAndCelsiusErr" [Show,Eq,Ord]

Interpolation FahrenheitAndCelsiusErr where
  interpolate InvalidFahrenheitValue  = "Invalid fahrenheit value"

0 FahrenheitAndCelsiusPull : Type -> Type -> Type
FahrenheitAndCelsiusPull o r = AsyncPull Poll o [Errno,FahrenheitAndCelsiusErr] r

0 FahrenheitAndCelsiusStream : Type -> Type
FahrenheitAndCelsiusStream o = AsyncPull Poll o [Errno,FahrenheitAndCelsiusErr] ()

record FahrenheitAndCelsius where
  constructor FAndC
  fahrenheitvalue : ByteString
  celsiusvalue : ByteString

convertFahrenheitToCelsius :  FahrenheitAndCelsiusPull ByteString (FahrenheitAndCelsiusStream ByteString)
                           -> FahrenheitAndCelsiusPull o (Maybe FahrenheitAndCelsius)
convertFahrenheitToCelsius fahrenheit = Prelude.do
  Right (h, rem) <- C.uncons fahrenheit
    | _ => pure Nothing
  let celsius = case parseDouble fahrenheit of
                  Nothing =>
                    fromString $ show 0.0
                  Just f  =>
                    fromString $ show $ (f - 32.0) * (5.0/9.0)
  pure $
    Just (FAndC fahrenheit celsius)

toCelsius :  FahrenheitAndCelsiusStream ByteString
          -> FahrenheitAndCelsiusPull o (Maybe FahrenheitAndCelsius)
toCelsius fahrenheitstr =
     breakAtSubstring pure "\n" fahrenheitstr
  |> convertFahrenheitToCelsius

echo :  Socket AF_INET
     -> FahrenheitAndCelsiusPull ByteString (Maybe FahrenheitAndCelsius)
     -> AsyncStream Poll [Errno] Void
echo cli p =
  extractErr FahrenheitAndCelsiusErr (writeTo Stdout (p >>= toCelsius)) >>= \case
    Left _   =>
         emit "Couldn't convert Fahrenheit to Celsius"
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
      |> echo cli

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
