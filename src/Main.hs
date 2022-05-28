module Main where
import qualified HGreet.Client as C
import qualified HGreet.Packet as P
main :: IO ()
main = do
    sockPath <- getEnv "GREETD_SOCK"
    withSocketDo sockPath $ \sock -> do
        handleResponse handler Nothing sock ["startx"]
    where
        handler :: Response -> IO C.PromptResult
        handler Success = putStr "Success" >> C.Success
        handler (AuthMessage t m) = case t of
            Visible  -> case m of
                "Username:" -> putStrFlush m >> getLine <&> C.Username
                _           -> putStrFlush m >> getLine <&> C.Auth
            Secret   -> putStrFlush m >> withEcho False getLine <&> C.Auth
            Info      -> putStrLn ("Info: " ++ m) >> C.Info
            ErrorType -> prompt (Error OtherError m)
        handler (Error t m) = case t of
            AuthError  -> putStrLn ("Authentication failed: " ++ m) >> C.Error
            OtherError -> putStrLn ("Error: " ++ m) >> C.Error

        putStrFlush :: String -> IO ()
        putStrFlush s = putStr s >> hFlush stdout

        withEcho :: Bool -> IO a -> IO a
        withEcho echo action = do
            old <- hGetEcho stdin
            bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action
