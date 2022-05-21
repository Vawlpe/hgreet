module IPCClient ( runIPCClient, awaitSendIPCPacket ) where
import Network.Socket
import Data.Maybe (fromJust)
import Network.Socket.ByteString (recv, sendAll, send)
import qualified Data.ByteString.Lazy as BL
import qualified Control.Exception as E
import IPCPacket ( Request(..), Response(..), AuthMessageType(..), ErrorType(..), encodeRequestPacket, decodeResponsePacket, decodeLen )

runIPCClient :: String -> (Socket -> IO a) -> IO a
runIPCClient path client = do
    E.bracket (open path) close client
  where
    open path = E.bracketOnError (sockOpen path) close $ \sock -> do
        connect sock $ SockAddrUnix path
        return sock
    sockOpen path = do
        sock <- socket AF_UNIX Stream defaultProtocol
        setSocketOption sock ReuseAddr 1
        return sock

sendIPCPacket :: Socket -> Request -> IO ()
sendIPCPacket sock req = sendAll sock $ encodeRequestPacket req

recvIPCPacket :: Socket -> IO Response
recvIPCPacket sock = do
    len <- recv sock 4
    let len' = decodeLen ( BL.fromStrict len ) :: Int
    packet <- recv sock len'
    return $ decodeResponsePacket packet

awaitSendIPCPacket :: Socket -> Request -> IO Response
awaitSendIPCPacket sock req = do
    sendIPCPacket sock req
    recvIPCPacket sock
