-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Collisions
import Input
import Model
import Score

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import qualified Data.Set as S
import Data.Char
import Data.Fixed

--------------------
-- Update loop - only update the game if the game is running, determined by the gameScreen we are in
--------------------
step :: Float -> GameState -> IO GameState
step secs gstate | gameScreen gstate == PlayScreen = handleIO (updateGame secs gstate { framesSinceLastSpawn = framesSinceLastSpawn gstate + 1} )
                 | otherwise = handleIO gstate

--------------------
-- Handles file operations (highscore load/save) based on flags in the gameState
--------------------
handleIO :: GameState -> IO GameState
handleIO gstate | gio == LoadHighscore = do
                                            hs <- readHighscore
                                            return gstate { gameHighscore = hs, gameIO = DoNothing }
                | gio == SaveHighscore = do
                                            saveHighscore gstate
                                            return gstate { gameIO = DoNothing }
                | otherwise = pure gstate

                where
                    gio = gameIO gstate

--------------------
-- Updates the AI, all game objects and then performs collision checking
--------------------
updateGame :: Float -> GameState -> GameState
updateGame secs gstate = spawnNewAsteroids secs $ performCollisions $ updateAI (gstate { gameObjects = updateGameObjects (gameObjects (handleAllKeys gstate)) })

--------------------
-- Periodically spawns new asteroids
--------------------
spawnNewAsteroids :: Float -> GameState -> GameState
spawnNewAsteroids secs gstate | framesSinceLastSpawn gstate > 100 = addRandomAsteroid gstate { framesSinceLastSpawn = 0 }
                              | otherwise = gstate

--------------------
-- Updates all the objects in the game (performs movement and roatation)
--------------------
updateGameObjects :: GameObjects -> GameObjects
updateGameObjects gobjs = gobjs { asteroids  = map move (asteroids gobjs),
                                  missiles   = updateMissiles (missiles gobjs),
                                  animations = updateAnimations (animations gobjs),
                                  enemies    = map move (enemies gobjs),
                                  player     = move (player gobjs) }
