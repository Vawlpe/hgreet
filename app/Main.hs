{-# LANGUAGE BlockArguments #-}
module Main where

import IPCPacket
import IPCClient (runIPCClient, awaitSendIPCPacket)
import System.Environment (getEnv)
main :: IO ()
main = do
    putStrLn "\n\n|--------------------------------------------------|"
    greetdSock <- getEnv "GREETD_SOCK"
    putStrLn $ "| [main] GREETD_SOCK: " ++ greetdSock
    putStrLn $ "| [main] Starting IPC Client @ " ++ greetdSock
    runIPCClient greetdSock \s -> do
        putStrLn $ "| [IPC] IPC Client started @ " ++ greetdSock

        putStrLn $ "| [IPC] Sending CreateSession packet to greetd and awaiting response"
        response <- awaitSendIPCPacket s $ CreateSession "dummy"
        putStrLn $ "| [IPC] Recieved Response: " ++ show response

        putStrLn $ "| [IPC] Sending CancelSession packet to greetd and awaiting response"
        response <- awaitSendIPCPacket s $ CancelSession
        putStrLn $ "| [IPC] Recieved Response: " ++ show response

        putStrLn $ "| [IPC] IPC Client finished @ " ++ greetdSock
    putStrLn "|--------------------------------------------------|\n\n"
