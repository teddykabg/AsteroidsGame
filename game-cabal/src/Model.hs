{-# LANGUAGE InstanceSigs #-}
module Model where

import Graphics.Gloss
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Graphics.Gloss.Interface.IO.Game

import qualified Data.Set as S
import qualified Graphics.Gloss.Data.Point.Arithmetic as GlossArithm
import System.Random

--------------------
-- Determines whether we want to save/load the highscores in the next update step
--------------------
data IOAction = SaveHighscore | LoadHighscore | DoNothing deriving Eq

--------------------
-- Defines the game screen we are currently on
--------------------
data GameScreen = MainScreen | PlayScreen | PauseScreen | EndScreen deriving Eq

--------------------
-- Contains the players name and score in the current game
--------------------
data GameStats = Stats { name :: String,
                         score :: Int }

--------------------
-- Encapsulates all the game objects (spaceships, asteroids, missiles) into one container
--------------------
data GameObjects = GameObjs { asteroids :: [Asteroid],
                              missiles :: [Missile],
                              enemies :: [Enemy],
                              player :: Spaceship,
                              animations ::[Animation]}

--------------------
-- Contains all information about the game and its state
--------------------
data GameState = State { gameScreen :: GameScreen,
                         randomGen     :: StdGen,
                         gameScore :: Int,
                         gameHighscore :: Int,
                         gameObjects :: GameObjects,
                         gameKeys :: S.Set SpecialKey,
                         gameIO :: IOAction,
                         framesSinceLastSpawn :: Int,
                         debug :: String }

--------------------
-- Returns the initial state of the game
--------------------
initialState :: IO GameState
initialState = do randGen <- getStdGen
                  let gs = State {
                    debug = "",
                    framesSinceLastSpawn = 0,
                    gameScreen = MainScreen, randomGen = randGen, gameScore = 0, gameHighscore = 0, gameKeys = S.empty, gameIO = LoadHighscore,
                    gameObjects = GameObjs { asteroids = [], missiles = [], enemies = [], player = createShip PlayerShip (640, 360), animations = [] }
                    }
                  return (addAsteroids gs 3) --start with 3 asteroids

--------------------
-- Resets the game state back to default
--------------------
resetToDefault :: GameState -> GameState
resetToDefault gstate = addAsteroids gs 3
  where gs = gstate { framesSinceLastSpawn = 0, gameScreen = MainScreen, gameScore = 0, gameKeys = S.empty, gameIO = LoadHighscore,
                      gameObjects = GameObjs { asteroids = [], missiles = [], enemies = [], player = createShip PlayerShip (640, 360), animations = [] }
                    }


--------------------
-- Tuple containing the X range of the game field
--------------------
gameRangeX :: (Float,Float)
gameRangeX = (0,1280)

--------------------
-- Tuple containing the Y range of the game field
--------------------
gameRangeY :: (Float,Float)
gameRangeY = (0,720)

--------------------
-- Get a random int in the specified range
--------------------
getRandomNo :: RandomGen g => (Int,Int) -> g -> (Int, g)
getRandomNo range g  = randomR range g

--------------------
-- Get a random float in the specified range
--------------------
getRandomNoF :: RandomGen g => (Float,Float) -> g -> (Float, g)
getRandomNoF range g  = randomR range g

--------------------
-- Returns random coordinate in the game field
--------------------
getRandomCoordinates :: RandomGen g => g -> (Point, g)
getRandomCoordinates g
  = ((x, y), g2)
    where (x, g1) = randomR gameRangeX g
          (y, g2) = randomR gameRangeY g1

--------------------
-- Terminates the current game, saves the highscore and displays the end screen
--------------------
gameOver :: GameState -> GameState
gameOver gstate = gstate { gameScreen = EndScreen, gameIO = SaveHighscore }

--------------------
-- Increments the current score
--------------------
incrementScore :: GameState -> Int -> GameState
incrementScore gstate n = gstate { gameScore = score, gameHighscore = max score high }
                        where
                           score = gameScore gstate + n
                           high = gameHighscore gstate


--------------------
--------------------
---- SHIP
--------------------
--------------------

--------------------
-- Defines whether an entity is controlled by a human or by AI (further AI types can be added)
--------------------
data ShipType = PlayerShip | EnemyShip deriving Eq

--------------------
-- Defines whether the ship has its engine engaged or idle (to display the flame)
--------------------
data ShipState = Idle | Propeling deriving Eq

--------------------
-- Defines a spaceship (player or enemy) and its properties
--------------------
data Spaceship = Ship { shipPosition :: Point,
                        shipSpeed :: Vector,
                        shipAngle :: Float,
                        shipType :: ShipType,
                        shipState :: ShipState }

--------------------
-- Max speed for player
--------------------
speedLimitPlayer :: Float
speedLimitPlayer = 10

--------------------
-- Max speed for enemies
--------------------
speedLimitEnemy :: Float
speedLimitEnemy = 6

--------------------
-- Tuple containing the speed range of a ship
--------------------
shipSpeedRange :: (Float,Float)
shipSpeedRange = (-7,7)

--------------------
-- Creates a new ship (player/enemy) at the given point
--------------------
createShip :: ShipType -> Point -> Spaceship
createShip tpe pt = Ship pt (0.0, 0.0) 0 tpe Idle
--------------------
-- Starts the engine of the ship to propel it forward
--------------------
propelShip :: Spaceship -> Spaceship
propelShip s = s { shipSpeed = newSpd, shipState = Propeling }
      where spd = shipSpeed s
            spdVal = rotateV (degToRad (shipAngle s)) (1, 0)
            limit = if shipType s == PlayerShip then speedLimitPlayer else speedLimitEnemy
            newSpd = limitSpeed (fst spd + fst spdVal, snd spd - snd spdVal) limit

--------------------
-- Stops the engine of the ship
--------------------
idleShip :: Spaceship -> Spaceship
idleShip s = s { shipState = Idle }

--------------------
-- Changes the spaceship angle and rotates the bounding box (graphic rotation is handled separately)
--------------------
rotateShip :: Spaceship -> Float -> Spaceship
rotateShip s rot = s { shipAngle = shipAngle s + rot }

--------------------
-- Turns the spaceship to face a specified point on screen
--------------------
turnToFace :: Spaceship -> Point -> Spaceship
turnToFace ship pt = ship { shipAngle = radToDeg (normalizeAngle(atan2 (shipy - pty) (ptx - shipx))) }
  where spos = shipPosition ship
        shipx = fst spos
        shipy = snd spos
        ptx = fst pt
        pty = snd pt

--------------------
-- Limits the speed to stay within the max speed
--------------------
limitSpeed :: Vector -> Float -> Vector
limitSpeed spd limit = (newX, newY)
  where x = fst spd
        y = snd spd
        newX = min (max x ((-1) * limit)) limit
        newY = min (max y ((-1) * limit)) limit

--------------------
-- Applies drag to speed, gradually slowing an entity down
--------------------
applyDrag :: Vector -> Vector
applyDrag spd = 0.95 GlossArithm.* spd




--------------------
--------------------
---- ASTEROID
--------------------
--------------------

--------------------
-- Defines an asteroid and its properties
--------------------
data Asteroid = As { asPosition :: Point,
                     asSpeed :: Vector,
                     asSize :: Float,
                     asExplosion :: Int}

--------------------
-- Tuple containing the speed range of an asteroid
--------------------
asteroidSpeedRange :: (Float,Float)
asteroidSpeedRange = (-3,3)

--------------------
-- Tuple containing the size range of an asteroid
--------------------
asteroidSizeRange :: (Float,Float)
asteroidSizeRange = (0.1,1)

--------------------
-- Creates an asteroid of the given size at the given point
--------------------
createAsteroid :: Float -> Point -> Vector -> Asteroid
createAsteroid size pt spd = As pt spd size (-1)

--------------------
-- Returns random speed for asteroids
-------------------
getRandomAsteroidSpeed :: RandomGen g => g -> (Vector, g)
getRandomAsteroidSpeed g
  = ((dx, dy), g2)
    where (dx, g1) = randomR asteroidSpeedRange g
          (dy, g2) = randomR asteroidSpeedRange g1

--------------------
-- Returns random size for an asteroid
--------------------
getRandomSize :: RandomGen g => g -> (Float, g)
getRandomSize = randomR asteroidSizeRange

--------------------
-- Explodes an asteroid into smaller ones (if it is sufficiently big)
--------------------
explodeAsteroid :: GameState -> Asteroid ->([Asteroid],StdGen)
explodeAsteroid gstate as | asSize as > (fst(asteroidSizeRange)+0.3) = (astPieces,updatedRand)
                          | otherwise                                = ([],(randomGen gstate))
           where (cnt,g1)           = getRandomNo  (2,3) (randomGen gstate)
                 (sgnx,g2)          = getRandomNoF (-1, 1) g1
                 (sgny,g3)          = getRandomNoF (-1, 1) g2
                 (spdx,g4)          = getRandomNoF (1, 4) g3
                 (spdy,g5)          = getRandomNoF (1,4) g4
                 (rnd1,g6)          = getRandomNoF (80,100) g5
                 (rnd2,g7)          = getRandomNoF (-80,-100) g6
                 (randSize, updatedRand) = getRandomNoF (0.1,asSize as) g7
                 astPieces          = map (\x -> createAsteroid randSize ((asPosition as) GlossArithm.+ (x,-x)) (sgnx * spdx, sgny * spdy)) (take cnt [rnd1,rnd2])

--------------------
-- Adds random asteroids to the game field
-------------------
addRandomAsteroid :: GameState -> GameState
addRandomAsteroid gs = gs{gameObjects = updatedObjects , randomGen = updatedRandom }
      where                               (pos,   g1)               = getRandomCoordinatesChecked gs -- get coordinates avoiding positions too near to the player
                                          (speed, g2)               = getRandomAsteroidSpeed g1
                                          (sizeA, updatedRandom)    = getRandomSize g2
                                          newAsteroid               = createAsteroid sizeA pos speed
                                          go                        = gameObjects gs
                                          updatedObjects            = go{ asteroids = newAsteroid : asteroids go}
                                          getRandomCoordinatesChecked::GameState -> (Point,StdGen)
                                          getRandomCoordinatesChecked gs | pointDistance pos1  (shipPosition (player go)) < 250 = getRandomCoordinatesChecked gs{randomGen = g0}
                                                                         | otherwise = (pos1,g0)
                                                  where (pos1,g0) = getRandomCoordinates (randomGen gs)
                                                        go        = gameObjects gs

--------------------
-- Spawns n random asteroids on the canvas
--------------------
addAsteroids :: GameState -> Int -> GameState
addAsteroids gstate n | n > 0 = addRandomAsteroid gstate
                      | otherwise = gstate

updateAsteroids :: [Asteroid] -> [Asteroid]
updateAsteroids asls = map move
                          (filter (\n -> not(asExplosion n == 0))
                          (map explosionGoingOn asls))
        where
          explosionGoingOn ast | (asExplosion ast) >0 = ast { asExplosion = asExplosion ast -1}
                               | otherwise            = ast



--------------------
--------------------
---- MISSILE
--------------------
--------------------

--------------------
-- Specifies whether the missile was shot by the player or an enemy
--------------------
data MissileType = PlayerMsl | EnemyMsl deriving Eq

--------------------
-- Defines how long a missile survives before it vanishes
--------------------
missileLifetime :: Float
missileLifetime = 75

--------------------
-- Defines a missile (a single shot) and its properties
--------------------
data Missile = Msl { mslPosition :: Point,
                     mslSpeed :: Vector,
                     mslLifetime :: Float,
                     mslType :: MissileType }

--------------------
-- Create a missile heading in the direction of the spaceship
--------------------
createMissile :: Spaceship -> Missile
createMissile ship = Msl mslposition mslspeed missileLifetime tpe
              where
              mslposition = shipPosition ship GlossArithm.+ (2 GlossArithm.* mslspeed)
              normalSpd = rotateV (degToRad (shipAngle ship)) (1, 0)
              mslspeed = (15 * fst normalSpd, (-15) * snd normalSpd)
              tpe = if shipType ship == PlayerShip then PlayerMsl else EnemyMsl


--------------------
-- Adds a missile shot by the player to the list of missiles in the gameObject
--------------------
addPlayerMissile :: GameState -> GameState
addPlayerMissile gstate = gstate { gameObjects = go { missiles = newMsl : missiles go} }
    where
      go     = gameObjects gstate
      newMsl = createMissile (player go)

--------------------
-- Adds a missile shot by an enemy to the list of missiles in the gameObject
--------------------
addEnemyMissile :: GameState -> Spaceship -> GameState
addEnemyMissile gstate enemy = gstate { gameObjects = go { missiles = newMsl : missiles go} }
    where
      go     = gameObjects gstate
      newMsl = createMissile enemy

--------------------
-- Updates the position of the missiles and remove them if their lifetimes are over
--------------------
updateMissiles :: [Missile] -> [Missile]
updateMissiles msls = map move
                      (filter (\n -> mslLifetime n > 0)
                      (map (\m -> m { mslLifetime = mslLifetime m - 1}) msls))
--------------------
-- Updates the position of the missiles and remove them if their lifetimes are over
--------------------
updateAnimations :: [Animation] -> [Animation]
updateAnimations anms = map move
                      (filter (\n -> not(anTimeout n == 0))
                      (map (\m -> m { anTimeout = anTimeout m - 1}) anms))


--------------------
--------------------
---- ENEMIES
--------------------
--------------------

--------------------
-- Enemy is a spaceship that counts time since it last took a shot at the player
--------------------
data Enemy = Enm { enmShip :: Spaceship,
                   shotTimeout :: Float }

--------------------
-- Probability (1 in N) that the enemy will shoot in this frame
--------------------
enemyShootProbability :: Int
enemyShootProbability = 10

--------------------
-- Time before the enemy can shoot again
--------------------
enemyShotTimeout :: Float
enemyShotTimeout = 100

--------------------
-- Creates a new enemy at the given location
--------------------
createEnemy :: Vector -> Enemy
createEnemy pos = Enm (createShip EnemyShip pos) 0

--------------------
-- Creates random enemies
--------------------
addRandomEnemies :: GameState -> GameState
addRandomEnemies gs = gs{gameObjects = updatedObjects , randomGen = updatedRandom }
    where (pos, updatedRandom)      = getRandomCoordinates (randomGen gs)
          go                        = gameObjects gs
          updatedObjects            = go { enemies = createEnemy pos : enemies go }

--------------------
-- AI determines the actions for enemy spaceships
--------------------
updateAI :: GameState -> GameState
updateAI gstate = enemiesShoot $ gstate { gameObjects = gobjs { enemies = map (enemyMovement gstate . increaseShotTimeout) enms }}
           where
            gobjs = gameObjects gstate
            enms = enemies gobjs
            plr = player gobjs

--------------------
-- Increases enemy shot timeout by 1
--------------------
increaseShotTimeout :: Enemy -> Enemy
increaseShotTimeout e = e { shotTimeout = shotTimeout e + 1 }

--------------------
-- Every ships attempts to shoot at the player
--------------------
enemiesShoot :: GameState -> GameState
enemiesShoot gstate = resetShootingEnemyTimeouts $ foldr (\e gs -> addEnemyMissile gs (enmShip e)) gstate (getShootingEnemies gstate)
              where gobjs = gameObjects gstate
                    enms = enemies gobjs

--------------------
-- Get a list of ships that will shoot at the player
--------------------
getShootingEnemies :: GameState -> [Enemy]
getShootingEnemies gstate = [e | e <- enms, shotTimeout e >= enemyShotTimeout ]
                            where gobjs = gameObjects gstate
                                  enms = enemies gobjs

--------------------
-- Enemies that will shoot get their timeouts reset
--------------------
resetShootingEnemyTimeouts :: GameState -> GameState
resetShootingEnemyTimeouts gstate = gstate { gameObjects = gobjs { enemies = map (\e -> if shotTimeout e >= enemyShotTimeout then e { shotTimeout = 0} else e) enms } }
                                    where gobjs = gameObjects gstate
                                          enms = enemies gobjs

--------------------
-- Based on player position, determine what the enemy should do (rotation, propeling, possibly shooting)
--------------------
enemyMovement :: GameState -> Enemy -> Enemy
enemyMovement gstate enemy = if distance > 200 then enemy { enmShip = propelShip turnedEnemy } else enemy { enmShip = turnedEnemy }
                            where
                              enship = enmShip enemy
                              gobjs = gameObjects gstate
                              playerPos = shipPosition (player gobjs)
                              distance = pointDistance playerPos (shipPosition enship)
                              turnedEnemy = turnToFace enship playerPos

--------------------
-- Computes the euclidian distance between two points
--------------------
pointDistance :: Point -> Point -> Float
pointDistance (x1, y1) (x2, y2) = sqrt (x' * x' + y' * y')
                                  where
                                    x' = x1 - x2
                                    y' = y1 - y2


--------------------
--------------------
---- ANIMATION
--------------------
--------------------
data Animation = Anm {  anPosition :: Point,
                        anTimeout :: Int,
                        anSpeed :: Vector}

--------------------
-- Creates a new animation at the given location
--------------------
createAnimation :: Point -> Animation
createAnimation pos  = Anm { anPosition = pos, anTimeout = 30, anSpeed = (5,5) }


--------------------
--------------------
---- MOVABLE
--------------------
--------------------

--------------------
-- Allows objects to move
--------------------
class Movable a where
  move :: a -> a

--------------------
-- Movable instances: Ship, Enemy, Asteroid, Missile
--------------------
instance Movable Spaceship where
  move :: Spaceship -> Spaceship
  move ship = ship { shipPosition = moveVec oldPos spd, shipSpeed = applyDrag (shipSpeed ship) }
    where
      oldPos = shipPosition ship
      spd = shipSpeed ship

instance Movable Enemy where
  move :: Enemy -> Enemy
  move enemy = enemy { enmShip = move (enmShip enemy) }

instance Movable Asteroid where
  move :: Asteroid -> Asteroid
  move as = as { asPosition = moveVec oldPos spd, asSpeed = asSpeed as }
    where
      oldPos = asPosition as
      spd = asSpeed as

instance Movable Missile where
  move :: Missile -> Missile
  move msl = msl { mslPosition = moveVec oldPos spd, mslSpeed = mslSpeed msl }
    where
      oldPos = mslPosition msl
      spd = mslSpeed msl
instance Movable Animation where
  move :: Animation -> Animation
  move anm = anm { anPosition = moveVec oldPos spd, anSpeed = anSpeed anm }
    where
        oldPos = anPosition anm
        spd = anSpeed anm

--------------------
-- Moves a point by a vector. Keeps the point within the screen boundaries, hardcoded for now (TODO)
--------------------
moveVec :: Point -> Vector -> Point
moveVec pt vec | newX < 0 = (1280, newY)
               | newX > 1280 = (0, newY)
               | newY < 0 = (newX, 720)
               | newY > 720 = (newX, 0)
               | otherwise = (newX, newY)
  where newX = fst pt + fst vec
        newY = snd pt + snd vec
