module Score where

import Model

import Control.Monad
import System.Directory

--------------------
-- Path to the highscore file
--------------------
highscoreFile :: String
highscoreFile = "highscore.txt"

--------------------
-- If the current score is higher than the saved highscore, rewrite it
--------------------
saveHighscore :: GameState -> IO ()
saveHighscore gstate = writeFile highscoreFile (show (gameHighscore gstate))

--------------------
-- Reads the current highscore stored in the highscore.txt file
--------------------
readHighscore :: IO Int
readHighscore = do
                  exists <- doesFileExist highscoreFile
                  Control.Monad.unless exists (writeFile highscoreFile "0")
                  txt <- readFile highscoreFile
                  return (read txt::Int)