module Collisions where

import Model

import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Interface.IO.Game
import qualified Graphics.Gloss.Data.Point.Arithmetic as GlossArithm

--------------------
-- Performs collisions on all game entities
--------------------
performCollisions :: GameState -> GameState
performCollisions gstate = collisionsAsteroidMissile (collisionsMissileEnemies (collisionsMissilePlayer (collisionsAsteroidPlayer gstate)))

--------------------
-- Resolves collisions between all missiles and all asteroids
--------------------
collisionsAsteroidMissile :: GameState -> GameState
collisionsAsteroidMissile gstate = incrementScore (gstate { gameObjects = newgo}) (length explosions)
    where
      go                    = gameObjects gstate
      ((newAsteroids,newMissile), explosions, newEnemies) = resolveCollisionAM gstate (asteroids go) (filter  (\m -> mslType m == PlayerMsl) (missiles go)) [] []
      newgo                 = go { asteroids = newAsteroids , missiles = newMissile ++ filter  (\m -> mslType m == EnemyMsl) (missiles go), enemies = newEnemies ++ enemies go ,animations= explosions++(animations go) }

      resolveCollisionAM :: GameState -> [Asteroid] -> [Missile] -> [Animation] -> [Enemy] -> (([Asteroid], [Missile]), [Animation], [Enemy])
      resolveCollisionAM  _ ax [] explod enms = ((ax, []), explod, enms)
      resolveCollisionAM  _ [] ms explod enms = (([], ms), explod, enms)
      resolveCollisionAM  gs (a:ax) (m:ms) explod enms | checkCollisionAM a m = ((explodedPieces ++ updasteroids, updmissiles), addedAnimation, addedEnemies)
                                                       | otherwise = ((a: updasteroids1, m:ms), explod, enms)
            where
              (explodedPieces,newRand) = explodeAsteroid gs a
              updatedGs = gs{randomGen = newRand}
              ((updasteroids,updmissiles),_,es) = resolveCollisionAM updatedGs ax ms addedAnimation es
              ((updasteroids1,updmissiles1),_,es1) = resolveCollisionAM updatedGs ax (m:ms) explod es1
              addedEnemies = if asSize a < 0.7 then enms else createEnemy (asPosition a) : enms
              addedAnimation = createAnimation(asPosition a):explod

--------------------
-- Resolves collisions between player and all asteroids
--------------------
collisionsAsteroidPlayer :: GameState -> GameState
collisionsAsteroidPlayer gstate | resolveCollisionPA ast pl = gameOver gstate
                                | otherwise = gstate
    where go = gameObjects gstate
          pl = player go
          ast = asteroids go

          resolveCollisionPA::[Asteroid] -> Spaceship -> Bool
          resolveCollisionPA xs pl = any (checkCollisionPA pl) xs

--------------------
-- Resolves collisions between all missiles and the player
--------------------

collisionsMissilePlayer :: GameState -> GameState
collisionsMissilePlayer gstate | resolveCollisionMP miss pl = gameOver gstate
                               | otherwise                             = gstate
    where go = gameObjects gstate
          pl = player go
          miss = missiles go

          resolveCollisionMP :: [Missile] -> Spaceship -> Bool
          resolveCollisionMP [] pl  = False
          resolveCollisionMP xs pl  = any (checkCollisionMP pl) xs

--------------------
-- Resolves collisions between all missiles and all enemies
--------------------
collisionsMissileEnemies :: GameState -> GameState
collisionsMissileEnemies gstate = incrementScore (gstate { gameObjects = newgo }) killed
    where
      go         = gameObjects gstate
      killed     = length (enemies go) - length newEnemies
      newEnemies = resolveCollisionMERec (enemies go) (filter (\m -> mslType m == PlayerMsl ) (missiles go))
      newgo      = go {enemies = newEnemies }

      resolveCollisionMERec :: [Enemy] -> [Missile] -> [Enemy]
      resolveCollisionMERec  sx []        = sx
      resolveCollisionMERec  [] _         = []
      resolveCollisionMERec (s:sx) (m:ms) | checkCollisionMP (enmShip s) m = resolveCollisionMERec sx ms
                                          | otherwise = s: resolveCollisionMERec sx (m:ms)

--------------------
-- Checks whether the player and an asteroid collide
--------------------
checkCollisionPA :: Spaceship -> Asteroid-> Bool
checkCollisionPA p a = magV (asPosition a GlossArithm.- shipPosition p) < (200 * asSize a)

--------------------
-- Checks whether the player and a missile collide
--------------------
checkCollisionMP :: Spaceship -> Missile-> Bool
checkCollisionMP p m = magV (mslPosition m GlossArithm.- shipPosition p) < 25

--------------------
-- Checks whether an asteroid and a missile collide
--------------------
checkCollisionAM :: Asteroid -> Missile-> Bool
checkCollisionAM a m = magV (asPosition a GlossArithm.- mslPosition m) < (280 * asSize a)
