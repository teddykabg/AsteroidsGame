{-# LANGUAGE InstanceSigs #-}
module View where

import Graphics.Gloss
import Model

--------------------
-- Main rendering method. Based on game state determines the correct screen to render
--------------------
view :: GameState -> IO Picture
view gstate = pure $ render gstate

--------------------
-- Type class for renderable objects
--------------------
class Renderable a where
  render :: a -> Picture

-- Renders the game itself
instance Renderable GameState where
  render :: GameState -> Picture
  render gstate = renderScreen (gameScreen gstate) gstate

-- Renders a spaceship (player/enemy)
instance Renderable Spaceship where
  render :: Spaceship -> Picture
  render p = translate x y (rotate angle (pictures [playerShape, flame]))
              where
                  c = if shipType p == PlayerShip then green else red
                  playerShape = Color c (Polygon [(-15,-10), (-15,10), (15,0)])
                  (x,y) = shipPosition p
                  angle = shipAngle p
                  flame = case shipState p of{Idle -> blank;
                                              Propeling -> Translate (-15) 0 (Scale 1 1 (Color yellow (Polygon [(4,7),(-4,0),(4, -7)])))}

-- Renders an asteroid                                              
instance Renderable Asteroid where
  render :: Asteroid -> Picture
  render a = translate x y (scale size size (pictures [asteroidShape]))
              where
                asteroidShape = Color cyan (Polygon [(0.0, 140.0),(140.0, 70.0), (140.0, -70.0),(0.0,-140.0), (-140.0, -70.0),(-140.0, 70.0)])
                (x,y)         = asPosition a
                size          = asSize a                                

-- Renders a missile                
instance Renderable Missile where
  render :: Missile -> Picture
  render m = translate (x+6.0) y (pictures [missileShape])
              where
                missileShape = Color yellow (Circle 3)
                (x,y)        = mslPosition m

-- Renders an animation of an asteroid breaking                
instance Renderable Animation where
  render :: Animation -> Picture
  render a = pictures pieces
              where
                missileShape =Color violet (Polygon [(-8.0,-2.0), (-8.0,2.0), (8.0,0.0)])
                (x,y)        = anPosition a
                pieces       = map ( \k -> translate (x+k) (y-k) missileShape ) (take (anTimeout a) [(-100),100,(-50),50,(-20),20,200,(-200),70,(-70)])

--------------------
-- Type class for renderable screens
--------------------
class RenderableScreen a where
  renderScreen :: a -> GameState -> Picture

instance RenderableScreen GameScreen where
  renderScreen :: GameScreen -> GameState -> Picture

  -- Renders the first screen with game logo and player name input
  renderScreen MainScreen gstate = translate (-280) 0 $ pictures[color white (text "Asteroids"), getHighscoreText gstate]

  -- Pause screen - displays the score and a message that the game is paused
  renderScreen PauseScreen gstate = translate (-230) 20 $ pictures [color white (text "Paused"), getStatsText gstate]

  -- End screen - displays "game over", players name and final score
  renderScreen EndScreen gstate = translate (-380) 20 $ pictures [color white (text "Game over!"), getHighscoreText gstate]

  -- Renders the game itself - player, enemies, asteroids, missiles and score
  renderScreen PlayScreen gState = translate (-640) (-360) $ pictures ( [mPlayer] ++ mEnemies ++ mAsteroids ++ mMissiles ++ mAnimations++ [mScore])
                              where
                                mPlayer       = render (player(gameObjects gState))
                                listEnemies   = enemies (gameObjects gState)
                                mEnemies      = map (render . enmShip) listEnemies
                                listAsteroids = asteroids (gameObjects gState)
                                mAsteroids    = map render listAsteroids
                                listMissiles  = missiles (gameObjects gState)
                                mMissiles     = map render listMissiles
                                listAnimations= animations (gameObjects gState)
                                mAnimations   = map render listAnimations
                                mScore        = getStatsTextGame gState

--------------------
-- Converts current game score into a picture
--------------------
getStatsText :: GameState -> Picture
getStatsText gstate = translate 0 (-120) $ color white (text ("Score:" ++ show (gameScore gstate)))

--------------------
-- Converts current game stats (name and score) into a picture
--------------------
getStatsTextGame :: GameState -> Picture
getStatsTextGame gstate = translate (20) (80) (scale 0.5 0.5 (getStatsText gstate))

--------------------
-- Converts current game highscore into a picutre
--------------------
getHighscoreText :: GameState -> Picture
getHighscoreText gstate = scale 0.5 0.5 $ translate 0 (-120) $ color white (text ("Highscore:" ++ show (gameHighscore gstate)))