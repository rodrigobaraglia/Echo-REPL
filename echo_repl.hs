module EchoREPL where

import Control.Applicative ()
import Data.Char (isSpace, toLower)

-----------------------------------------------------------------------------------------------
--Entry point:
-----------------------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn welcomeMessage
  repl defaultMode defaultPrompt

-----------------------------------------------------------------------------------------------
--Application:
-----------------------------------------------------------------------------------------------
repl :: String -> String -> IO ()
repl mode prevInput = do
  putStr $ prevInput ++ ">>>"
  newInput <- getNewInput

  let command = map toLower newInput

  if command == commandExit
    then exit
    else updateRepl command prevInput newInput mode

-----------------------------------------------------------------------------------------------
--Procedures:
-----------------------------------------------------------------------------------------------
getNewInput :: IO String
getNewInput = unwords . words <$> getLine

exit :: IO ()
exit = do
  putStrLn exitMessage
  return ()

updateRepl :: String -> String -> String -> String -> IO ()
updateRepl command prevInput newInput mode = do
  let modeUpdate
        | command == commandReverseMode = command
        | command == commandNormalMode = command
        | otherwise = mode

  let promptUpdate = if modeUpdate == command then prevInput else newInput

  if modeUpdate == commandReverseMode
    then repl modeUpdate (reverse promptUpdate)
    else repl modeUpdate promptUpdate

---------------------------------------------------------------------------------------------
--Commands:
---------------------------------------------------------------------------------------------
commandExit :: String
commandExit = ":exit"

commandReverseMode :: String
commandReverseMode = ":reverse"

commandNormalMode :: String
commandNormalMode = ":normal"

-------------------------------------------------------------------------------------------
--Default Values:
-------------------------------------------------------------------------------------------
defaultPrompt :: String
defaultPrompt = ""

defaultMode :: String
defaultMode = ":normal"

-------------------------------------------------------------------------------------------
--Messages:
-------------------------------------------------------------------------------------------
welcomeMessage :: String
welcomeMessage =
  "Welcome to the Echo REPL. To enter reverse mode type \
  \':reverse'. Type ':exit' if you want to leave the Echo REPL."

exitMessage :: String
exitMessage = "You are leaving the Echo REPL."

-------------------------------------------------------------------------------------------
