module Main where

import Controller
import Input
import Model
import View

import Graphics.Gloss.Interface.IO.Game

--------------------
-- Initialize the main game loop, canvas, rendering, etc.
--------------------
main :: IO ()
main = do is <- initialState
          playIO (InWindow "Asteroids" (1280, 720) (0, 0)) -- Or FullScreen
            black            -- Background color
            30               -- Frames per second
            is               -- Initial state
            view             -- View function
            input            -- Event function
            step             -- Step function