module HGreet.Client ( run, awaitSend, send, recv ) where
import Data.Maybe (fromJust)
import qualified Data.ByteString.Lazy as BL
import qualified Control.Exception as E
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import HGreet.Packet ( Request(..), Response(..), AuthMessageType(..), ErrorType(..), encodeRequest, decodeResponse, decodeLen )

run :: String -> (NS.Socket -> IO a) -> IO a
run path client = do
    E.bracket (open path) NS.close client
  where
    open path = E.bracketOnError (sockOpen path) NS.close $ \sock -> do
        NS.connect sock $ NS.SockAddrUnix path
        return sock
    sockOpen path = do
        sock <- NS.socket NS.AF_UNIX NS.Stream NS.defaultProtocol
        NS.setSocketOption sock NS.ReuseAddr 1
        return sock

send :: NS.Socket -> Request -> IO ()
send sock req = NSB.sendAll sock $ encodeRequest req

recv :: NS.Socket -> IO Response
recv sock = do
    len <- NSB.recv sock 4
    let len' = decodeLen ( BL.fromStrict len ) :: Int
    packet <- NSB.recv sock len'
    return $ decodeResponse packet

awaitSend :: NS.Socket -> Request -> IO Response
awaitSend sock req = do
    send sock req
    recv sock
