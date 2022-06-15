{-# LANGUAGE LambdaCase 
           , DeriveGeneric
#-}
{-|
Module         : HGreet.Client
Description    : Simplified communication with the greetd daemon.
Copyright      : (c) Hazel (Vawlpe), 2022
License        : GPL-3.0-or-later
Maintainer     : vawlpe@gmail.com
Stability      : experimental
Portability    : Linux

To use this module, first get the path to the socket of the greetd daemon from the environment variable @GREETD_SOCK@. This requires the greetd daemon to be running.
Then you can communicate with greetd using the `withSocketDo` function of this module, passing it a callback function that will have direct acccess to the open socket.
For a simplified communication scheme you can implement a @handler@ and pass it to @handleResponse@ alongside the socket and command to run on successful authentication.
-}
module HGreet.Client (withSocketDo, send, recv, handleResponse, PromptResult(..)) where

import Data.Maybe (fromJust)
import Data.Functor ((<&>))
import Control.Exception (bracket_)
import Control.Concurrent (threadDelay)
import GHC.Generics (Generic)
import System.IO
import System.Exit
import qualified Data.ByteString.Lazy as BL
import qualified Control.Exception as E
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified HGreet.Packet as P ( Request(..), Response(..), AuthMessageType(..), ErrorType(..), encodeRequest, decodeResponse, decodeLen )

{-
  The `withSocketDo` function takes a to the socket of the greetd daemon and a callback function which will have access to the open socket.
  Within the callback function, you may use the send and recv functions and the HGreet.Packet module to communicate with the greetd daemon directly.
  Alternatively you can use the `handleResponse` function to implement the default login routine of greetd given a handler function.
  For examples on how to do both of these, see the "hagreety" package.
-}
withSocketDo :: String              -- ^ Path to the socket of the greetd daemon, usually found in the environment variable @GREETD_SOCK@
             -> (NS.Socket -> IO a) -- ^ Callback function that will have direct access to the open socket.
             -> IO a                -- ^ Result of the callback function as an IO action.
withSocketDo path client = do
    E.bracket (open path) NS.close client
  where
    open path = E.bracketOnError (sockOpen path) NS.close $ \sock -> do
        NS.connect sock $ NS.SockAddrUnix path
        return sock
    sockOpen path = do
        sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
        NS.setSocketOption sock NS.ReuseAddr 1
        return sock

{-
 Send a `HGreet.Packet.Request` to the greetd daemon given an open socket.
 Usually used within the callback of a `withSocketDo` function to implement low level communication with the greetd daemon.
-}
send :: NS.Socket -- ^ Open socket to the greetd daemon, usually obtained from the callback of a `withSocketDo` function.
     -> P.Request -- ^ `HGreet.Packet.Request` to send to the greetd daemon.
     -> IO ()     -- ^ Empty IO action result.
send sock req = NSB.sendAll sock $ P.encodeRequest req

{-
 Receive a `HGreet.Packet.Response` from the greetd daemon given an open socket.
 Usually used within the callback of a withSocketDo function to implement low level communication with the greetd daemon.
-}
recv :: NS.Socket     -- ^ Open socket to the greetd daemon, usually obtained from the callback of a `withSocketDo` function.
     -> IO P.Response -- ^ `HGreet.Packet.Response` received from the greetd daemon as an IO action.
recv sock = do
    len <- NSB.recv sock 4
    let len' = P.decodeLen ( BL.fromStrict len ) :: Int
    packet <- NSB.recv sock len'
    return $ P.decodeResponse packet

{-
 Generic default login routine for greetd.
 Allows you to simply slap a handler function to deal with user input and have a working greeter with minimal effort.
 Currently kinda broken, will fix soon.
-}
handleResponse :: (P.Response -> IO PromptResult) -- ^ Handler function that will be called for every response from greetd.
               -> P.Response                      -- ^ Response from greetd that will be passed to the handler function.
               -> NS.Socket                       -- ^ Open socket to the greetd daemon, usually obtained from the callback of a `withSocketDo` function.
               -> [String]                        -- ^ List of strings to pass as the command to execute to start session after authentication.
               -> IO ()                           -- ^ Empty IO action result.
handleResponse handler resp sock cmd =
    handler resp >>= \case
        Error -> do
            send sock P.CancelSession
            threadDelay 1500000
            exitFailure
        Auth msg -> do
            send sock $ P.PostAuthMessageResponse $ Just msg
            rsp <- recv sock
            handleResponse handler rsp sock cmd
        Info -> do 
            rsp <- recv sock
            handleResponse handler rsp sock cmd
        Success -> do
            send sock $ P.StartSession cmd
            threadDelay 500000
            exitSuccess

{- 
  Prompt result type for handler functions that work with `handleResponse`.
-}
data PromptResult
    = Success         -- ^ Successful prompt result.
    | Error           -- ^ Error prompt result.
    | Info            -- ^ Info prompt result.
    | Auth String     -- ^ Auth prompt result.
    deriving (Generic, Eq, Show)
