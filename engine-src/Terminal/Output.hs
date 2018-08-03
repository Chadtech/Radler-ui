module Terminal.Output 
    ( say
    , newLine
    , separator
    ) where

import System.Process (callCommand)


say :: String -> IO ()
say str = do
    callCommand "osascript -e \"set volume 2\" "
    callCommand ("say " ++ str)
    callCommand "osascript -e \"set volume 4\" "


newLine :: IO ()
newLine =
    putStrLn ""


separator :: IO ()
separator =
    putStrLn "-------"