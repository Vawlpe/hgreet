module Main where
import System.Environment (getEnv)
import Data.Functor (($>), (<&>))
import Network.Socket
import Control.Exception (bracket_)
import System.IO
import qualified HGreet.Client as C (send, recv, handleResponse, withSocketDo, PromptResult(..))
import qualified HGreet.Packet as P

main :: IO ()
main = do
    sockPath <- getEnv "GREETD_SOCK"
    C.withSocketDo sockPath $ \sock -> do
        C.handleResponse handler Nothing sock ["ls"]
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
            P.ErrorType -> putStrLn ("Error: " ++ m) >> return C.Error
        handler (P.Error t m) = case t of
            P.AuthError  -> putStrLn ("Authentication failed: " ++ m) $> C.Error
            P.OtherError -> putStrLn ("Error: " ++ m) >> return C.Error

        putStrFlush :: String -> IO ()
        putStrFlush s = putStr s >> hFlush stdout

        withEcho :: Bool -> IO a -> IO a
        withEcho echo action = do
            old <- hGetEcho stdin
            bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
