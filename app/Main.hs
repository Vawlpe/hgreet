{-# LANGUAGE  BlockArguments, LambdaCase #-}
module Main where

import IPCPacket           (Request (..), Response (..))
import IPCClient           (runIPCClient, awaitSendIPCPacket, sendIPCPacket, recvIPCPacket)
import System.Environment  (getEnv, getArgs)
import System.Exit         (exitFailure, exitSuccess)
import System.IO           (hFlush, stdout)
import Data.Aeson          hiding (Error, Success)
import Network.Socket      (Socket)

main :: IO ()
main = do
    putStrLn "\n\n|--------------------------------------------------|"
    greetdSock <- getEnv "GREETD_SOCK"
    putStrLn $ "| [main] GREETD_SOCK: " ++ greetdSock
    putStrLn $ "| [main] Starting IPC Client @ " ++ greetdSock
    runIPCClient greetdSock \s -> do
        putStrLn          $ "| [IPC] IPC Client started @ " ++ greetdSock

        putStr            $ "| [IPC] Username: "
        hFlush            $ stdout
        username          <- getLine

        putStrLn          $ "| [IPC] Sending CreateSession packet to GREETD"
        response          <- awaitSendIPCPacket s $ CreateSession username
        handleResponse s  $ response
        
        putStrLn          $ "| [IPC] Sending StartSession packet to GREETD"
        response          <- awaitSendIPCPacket s $ StartSession ["startx"]
        handleResponse s  $ response

        putStrLn          $ "| [IPC] IPC Client finished @ " ++ greetdSock
    putStrLn "|--------------------------------------------------|\n\n"
    exitSuccess
    
    where
        handleResponse :: Socket -> Response -> IO ()
        handleResponse s = \case
            AuthMessage t m  -> do
                putStr            $ "| [IPC] Auth  (" ++ show t ++ "): " ++ m
                hFlush            $ stdout
                auth              <- getLine
                putStrLn          $ "| [IPC] Sending Auth packet to GREETD"
                response          <- awaitSendIPCPacket s $ PostAuthMessageResponse (Just auth)
                handleResponse s  $ response

            Success          -> putStrLn  $ "| [IPC] Success!"
            Error t m        -> do
                putStrLn  $ "| [IPC] Error (" ++ show t ++ "): " ++ m
                putStrLn          $ "| [IPC] Sending CancelSession packet to GREETD"
                response          <- awaitSendIPCPacket s $ CancelSession
                handleResponse s  $ response
                putStrLn          $ "| [IPC] IPC Client failed"
                exitFailure
