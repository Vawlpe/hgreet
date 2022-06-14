{-
Module      : hgreet-example
Description : A simple example greeeter made with hgreet based of agreety / agetty.
Copyright   : (c) Hazel (Vawlpe), 2022
License     : GPL-3.0-or-later
Maintainer  : vawlpe@gmail.com
Stability   : experimental
Portability : Linux

This is only an example greeter to show you the general steps involved with creating a working greeter using @hgreet@ and @greetd@
Please do NOT use this as your default systemwide greeter, I cannot guarantee the security or reliability of this example.
-}
module Main where
import System.Environment (getEnv, getArgs)
import Data.Functor (($>), (<&>))
import Control.Exception (bracket_)
import System.IO
import qualified HGreet.Client as C (send, recv, handleResponse, withSocketDo, PromptResult(..))
import qualified HGreet.Packet as P

-- * Simple command line greeter
-- | This is the main function of the greeter.
-- It will take a command to run to start a session after the greeter has successfully authenticated a user.
-- Usage is: @example <command>@ where <command> is the command to run to start the session and it's parameters (if any), separated by spaces, no quotes.
-- Example: @example startx@
-- This will start an x11 session with the command @startx@.
--
-- The greeter also takes the socket to connect to from the @GREETD_SOCK@ environment variable, which should be present if greetd is running correctly.
--
-- The greeter will simply run a `HGreet.Client.handleResponse` loop given a handler function and an open socket socket, within the callback of a `HGreet.Client.withSocketDo` given the path to the socket.
--
-- For direct communcation with the socket rather then a handler, see the example bellow.
-- $directCom
main :: IO ()
main = do
    cmd <- getArgs
    sockPath <- getEnv "GREETD_SOCK"
    C.withSocketDo sockPath $ \sock -> C.handleResponse handler Nothing sock cmd
    where
        handler :: P.Response -> IO C.PromptResult
        handler P.Success = putStr "Success" >> return C.Success
        handler (P.AuthMessage t m) = case t of
            P.Visible  -> putStrFlush m >> getLine <&> case m of
                "Username:" -> C.Username
                _           -> C.Auth
            P.Secret    -> do
                putStrFlush m
                inp <- withEcho False getLine
                putChar '\n'
                return $ C.Auth inp
            P.Info      -> putStrLn ("Info: " ++ m) >> return C.Info
            P.ErrorAuth -> putStrLn ("Error: " ++ m) >> return C.Error
        handler (P.Error t m) = case t of
            P.AuthError  -> putStrLn ("Authentication failed: " ++ m) $> C.Error
            P.OtherError -> putStrLn ("Error: " ++ m) >> return C.Error

        putStrFlush :: String -> IO ()
        putStrFlush s = putStr s >> hFlush stdout

        withEcho :: Bool -> IO a -> IO a
        withEcho echo action = do
            old <- hGetEcho stdin
            bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

-- $directCom
-- | This is an example of how to directly communicate with the socket rather then using a handler.
-- @
-- import qualified HGreet.Client as C (send, recv, withSocketDo)
-- import qualified HGreet.Packet as P (Request(..), Response(..), AuthMessageType(..), ErrorType(..))
-- main :: IO ()
-- main = do
--   sockPath <- getEnv "GREETD_SOCK"
--   C.withSocketDo sockPath $ \sock -> do
--     putStr "Username: "
--     hFlush stdout
--     C.send sock $ P.CreateSession =<< getLine
--     C.recv sock >>= \case
--       P.Success         -> ...
--       P.AuthMessage t m -> case t of
--         P.Visible -> ...
--         P.Secret  -> ...
--         P.Info    -> ...
--         P.ErrorType -> ...
--      P.Error t m -> case t of
--        P.AuthError  -> ...
--        P.OtherError -> ...
--      _ -> ... -- Should not happen
-- @
