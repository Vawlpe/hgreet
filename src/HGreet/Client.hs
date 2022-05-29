{-# LANGUAGE LambdaCase 
           , DeriveGeneric
#-}
module HGreet.Client ( withSocketDo, send, recv, handleResponse, PromptResult(..) ) where
import Data.Maybe (fromJust)
import Data.Functor ((<&>))
import Control.Exception (bracket_)
import Control.Concurrent (threadDelay)
import GHC.Generics (Generic)
import System.IO
import qualified Data.ByteString.Lazy as BL
import qualified Control.Exception as E
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified HGreet.Packet as Packet ( Request(..), Response(..), AuthMessageType(..), ErrorType(..), encodeRequest, decodeResponse, decodeLen )

withSocketDo :: String -> (NS.Socket -> IO a) -> IO a
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

send :: NS.Socket -> Packet.Request -> IO ()
send sock req = NSB.sendAll sock $ Packet.encodeRequest req

recv :: NS.Socket -> IO Packet.Response
recv sock = do
    len <- NSB.recv sock 4
    let len' = Packet.decodeLen ( BL.fromStrict len ) :: Int
    packet <- NSB.recv sock len'
    return $ Packet.decodeResponse packet

handleResponse :: (Packet.Response -> IO PromptResult) -> Maybe Packet.Response -> NS.Socket -> [String] -> IO ()
handleResponse handler resp sock cmd = case resp of
    Nothing -> handleResponse handler (Just (Packet.AuthMessage Packet.Visible "Username:")) sock cmd
    Just resp -> handler resp >>= \case
        Error -> do
            threadDelay 2000000
            handleResponse handler (Just (Packet.AuthMessage Packet.Visible "Username:")) sock cmd
        Username msg -> do
            send sock $ Packet.CreateSession msg
            rsp <- recv sock 
            handleResponse handler (Just rsp) sock cmd
        Auth msg -> do
            send sock $ Packet.PostAuthMessageResponse $ Just msg
            rsp <- recv sock
            handleResponse handler (Just rsp) sock cmd
        Info -> do 
            rsp <- recv sock
            handleResponse handler (Just rsp) sock cmd
        Success -> send sock $ Packet.StartSession cmd

data PromptResult
    = Success
    | Error
    | Info
    | Username String
    | Auth String
    deriving (Generic, Eq, Show)
