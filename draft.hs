--------------------
------ Model -------
--------------------

--------------------
-- Defines the game screen we are currently on
--------------------
data GameScreen = MainScreen | PlayScreen | PauseScreen | EndScreen

--------------------
-- Defines whether an entity is controlled by a human or by AI (further AI types can be added)
--------------------
data PlayerType = Human | AI

--------------------
-- Defines the bounding box for an entity, simplifies collision detection
--------------------
data BoundingBox = Bound { leftUpper :: Point,
                           rightBottom :: Point }

--------------------
-- Defines a spaceship (player or enemy) and its properties
--------------------
data Spaceship = Ship { shipPosition :: Point,
                        shipSpeed :: Int,
                        angle :: Float,
                        shipBoundingBox :: BoundingBox,
                        bulletTimeout :: Int,
                        playerType :: PlayerType }

--------------------
-- Defines an asteroid and its properties
--------------------
data Asteroid = As { asSize :: Int,
                     asPosition :: Point,
                     asBoundingBox :: BoundingBox,
                     asSpeed :: Vector }

--------------------
-- Defines a missile (a single shot) and its properties
--------------------
data Missile = Msl { mslLifetime :: Int,
                     mslPosition :: Point, 
                     mslBoundingBox :: BoundingBox,
                     mslSpeed :: Vector }

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
                              enemies :: [Spaceship],
                              player :: Spaceship }

--------------------
-- Contains all information about the game and its state
--------------------
data GameState = State { gameScreen :: GameScreen,
                         gameStats :: GameStats,
                         gameObjects :: GameObjects }
--------------------
-- Returns the initial state of the game
--------------------
initialState :: GameState

--------------------
-- Creates a new ship (player/enemy) at the given point
--------------------
createShip :: PlayerType -> Point -> Spaceship

--------------------
-- Creates an asteroid of the given size at the given point
--------------------
createAsteroid :: Int -> Point -> Vector -> Asteroid

--------------------
-- Create a missile heading in the direction of the spaceship
--------------------
createMissile :: Spaceship -> Missile

--------------------
-- Adds a missile to the list of missiles in the gameObject
--------------------
addMissile :: GameObjects -> Missile -> GameObjects

--------------------
-- Sets the ships speed to propel it forward
--------------------
propelShip :: Spaceship -> Spaceship

--------------------
-- Moves an object in the direction of the specified vector
--------------------
move :: Point -> Vector -> Point

--------------------
-- Checks whether two bounding boxes collide
--------------------
checkCollision :: BoundingBox -> BoundingBox -> Bool

--------------------
-- Changes the spaceship angle and rotates the bounding box (graphic rotation is handled separately)
--------------------
rotateShip :: Spaceship -> Float -> Spaceship

--------------------
-- Whenever an asteroid is hit, explode it into smaller asteroids (or none if its too small)
--------------------
explodeAsteroid :: Asteroid -> IO [Asteroid]

--------------------
-- Updates the position of the asteroids
--------------------
updateAsteroids :: [Asteroid] -> [Asteroid]

--------------------
-- Updates the position of the missiles and remove them if their lifetimes are over
--------------------
updateMissiles :: [Missile] -> [Missile]


--------------------
------- View -------
--------------------

--------------------
-- Main rendering method. Based on game state determines the correct screen to render
--------------------
view :: GameState -> IO Picture

--------------------
-- Renders the first screen with game logo and player name input
--------------------
renderMainScreen :: GameState -> Picture

--------------------
-- Renders the game itself - player, enemies, asteroids, missiles and score
--------------------
renderPlayScreen :: GameState -> Picture

--------------------
-- Pause screen - shows the score and a message that the game is paused
--------------------
renderPauseScreen:: GameState -> Picture

--------------------
-- End screen - displays "game over", players name and final score
--------------------
renderEndScreen :: GameState -> Picture

--------------------
-- Converts current game stats (name and score) into a picture
--------------------
getStatsText :: GameStats -> Picture


--------------------
---- Controller ----
--------------------

--------------------
-- Update loop
--------------------
step :: Float -> GameState -> IO GameState

--------------------
-- Handle user input
--------------------
input :: Event -> GameState -> IO GameState

--------------------
-- Handle keypresses
--------------------
inputKey :: Event -> GameState -> GameState

--------------------
-- Handle mouse events (for the mouse input bonus)
--------------------
inputMouse :: Event -> GameState -> GameState

--------------------
-- AI determines the actions for enemy spaceships
--------------------
updateAI :: GameObjects -> GameObjects

--------------------
-- Performs collisions on all game entities
--------------------
performCollisions :: GameState -> GameState