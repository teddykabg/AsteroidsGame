module Input where

import Model

import qualified Data.Set as S
import Data.Char

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

--------------------
-- Handle user input
--------------------
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

--------------------
-- Handle keypresses
--------------------
inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey key) Down _ _) gstate = case key of

  -- ESC to exit game from pause screen or return to main screen from end screen
  KeyEsc -> case gameScreen gstate of
              PauseScreen -> gstate { gameScreen = EndScreen, gameIO = SaveHighscore }
              EndScreen -> resetToDefault gstate
              _ -> gstate

  -- Enter on main screen to start game
  KeyEnter -> case gameScreen gstate of
                MainScreen -> gstate { gameScreen = PlayScreen }
                EndScreen -> resetToDefault gstate
                _ -> gstate

  -- Space to shoot a missile
  KeySpace -> case gameScreen gstate of
                PlayScreen -> addPlayerMissile gstate
                _ -> gstate

  -- All other keys get added to keypress array
  _ -> gstate { gameKeys = S.insert key (gameKeys gstate) }

inputKey (EventKey (Char c) Down _ _) gstate = case toUpper c of
  -- P to pause/unpause the game
  'P' -> case gameScreen gstate of
          PlayScreen -> gstate { gameScreen = PauseScreen }
          PauseScreen -> gstate { gameScreen = PlayScreen }
          _ -> gstate

  'A' -> addRandomAsteroid gstate -- Adds random asteroids TODO: to remove just for debug
  'E' -> addRandomEnemies gstate -- Adds random enemies TODO: to remove just for debug

  _ -> gstate

-- Handle keys being released. Also specific events upon key release
inputKey (EventKey (SpecialKey key) Up _ _) gstate = case key of
  -- workaround to disable the flame here. TODO make this less ugly
  KeyUp -> case gameScreen gstate of
    PlayScreen -> gstate { gameObjects = (gameObjects gstate) { player = idleShip (player (gameObjects gstate)) }, gameKeys = S.delete key (gameKeys gstate) }
    _ -> gstate { gameKeys = S.delete key (gameKeys gstate) }

  _ -> gstate { gameKeys = S.delete key (gameKeys gstate) }

-- Handling mouse presses for left (propel ship) and right (shoot) buttons
inputKey (EventKey (MouseButton LeftButton) Down _ (_, _)) gstate = gstate { gameKeys = S.insert KeyUp (gameKeys gstate) } --simulate up arrow press
inputKey (EventKey (MouseButton LeftButton) Up _ (_, _)) gstate = gstate { gameObjects = (gameObjects gstate) { player = idleShip (player (gameObjects gstate)) }, gameKeys = S.delete KeyUp (gameKeys gstate) } --simulate up arrow release + flame workaround (TODO make prettier)

inputKey (EventKey (MouseButton RightButton) Down _ (_, _)) gstate = addPlayerMissile gstate --shoot

-- When mouse movement is detected, orient the player to face the mouse. TODO mouse offset is hardcoded, resolution changes are not allowed
inputKey (EventMotion (ptx, pty)) gstate = gstate { gameObjects = (gameObjects gstate) { player = turnToFace (player (gameObjects gstate)) (ptx + 640, pty + 360) } }

-- All other cases => do nothing
inputKey _ gstate = gstate

--------------------
-- Iterate over all keys that are pressed down and perform their respective actions
--------------------
handleAllKeys :: GameState -> GameState
handleAllKeys gstate = foldr handleKey gstate (gameKeys gstate)

--------------------
-- Handle individual keys and their respective actions
--------------------
handleKey :: SpecialKey -> GameState -> GameState
-- Up = propel ship
handleKey KeyUp gstate = case gameScreen gstate of
  PlayScreen -> gstate { gameObjects = (gameObjects gstate) { player = propelShip (player (gameObjects gstate)) } }
  _ -> gstate

-- Left = rotate left
handleKey KeyLeft gstate = case gameScreen gstate of
  PlayScreen -> gstate { gameObjects = (gameObjects gstate) { player = rotateShip (player (gameObjects gstate)) (-5) } }
  _ -> gstate

-- Right = rotate right
handleKey KeyRight gstate = case gameScreen gstate of
  PlayScreen -> gstate { gameObjects = (gameObjects gstate) { player = rotateShip (player (gameObjects gstate)) 5 } }
  _ -> gstate

-- default case for everything else
handleKey _ gstate = gstate