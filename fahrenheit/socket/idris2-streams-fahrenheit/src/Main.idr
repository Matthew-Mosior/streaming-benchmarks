module Main

import Data.String
import Data.Vect
import FS.Posix
import FS.Posix.Internal
import FS.Socket
import IO.Async.Loop.Epoll
import IO.Async.Loop.Posix
import System

%default total

%language ElabReflection

addr :  Bits16
     -> IP4Addr
addr = IP4 [127,0,0,1]

toCelsius :  ByteString
          -> Maybe Double
toCelsius bs =
  case parseDouble bs of
    Nothing =>
      Nothing
    Just f  =>
      Just $ (f - 32.0) * (5.0/9.0)

handler :  HSum [Errno]
        -> AsyncStream e [Errno] (Either Errno a)
handler (Here x) = emit (Left x)

logRes :  Either Errno ByteString
       -> Async Poll [Errno] ()
logRes (Left x)  =
  stderrLn "Error: \{x}"
logRes (Right x) =
  fwritenb Stdout x

connectTo :  (d : Domain)
          -> SockType
          -> Addr d
          -> Async Poll [Errno] (Socket d)
connectTo d t addr = do
  sock <- socketnb d t
  connectnb sock addr
  pure sock

echoClient :  Bits16
           -> AsyncStream Poll [Errno] (Either Errno ByteString)
echoClient port =
  handleErrors handler $
    bracket
      (connectTo AF_INET SOCK_STREAM $ addr port)
      (\cl => close' cl) $ \cl =>
           (eval $ fwritenb Stdout "hello from \{show $ fileDesc cl}\n")
        |> P.bind (\_ =>
                       bytes cl 0xfff
                    |> lines
                    |> C.mapMaybe toCelsius
                    |> C.mapOutput (fromString . show)
                    |> unlines
                  )
        |> P.mapOutput Right

covering
prog :  List String
     -> AsyncStream Poll [Errno] Void
prog []        =
  throw EINVAL
prog (_::port) =
  case last' port of
    Nothing    =>
      stderrLn "Usage: pack run idris2-streams-fahrenheit.ipkg [port]"
    Just port' =>
      timeout 20.s (echoClient (cast {to=Bits16} port') |> foreach logRes)

covering
runProg :  AsyncStream Poll [Errno] Void
        -> IO ()
runProg prog =
  simpleApp $
    mpull ( handle [stderrLn . interpolate] prog
          )

covering
main : IO ()
main = getArgs >>= runProg . prog
