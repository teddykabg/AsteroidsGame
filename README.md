# AsteroidsGame
Final project course of Functional Programming Utrecht University
# Asteroids

## Data and architecture design

By Matej Havranek & Teddy Gyabaah

## Introduction and Rules

#### The player controls a spaceship which can move around the screen. The spaceship has

#### to avoid the asteroids that pop up on the screen, and can destroy them by shooting at

#### them, at which point they split into smaller asteroids.

#### Each asteroid has a chance of having an enemy spaceship inside. When freed by

#### destroying the asteroid, it flies around and tries to shoot the player.

#### The goal of the player is to gain as much points as possible by destroying asteroids and

#### killing the enemies in the game.

## Design

### Data structures

#### The full state of the game is represented using a GameState object that contains three

#### fundamental states of our game, namely the state of the Game screen, the stats situation

#### and the actual objects involved in the game in a precise moment.

#### data​ ​GameState​ = ​State​ { ​gameScreen​ :: ​GameScreen​,


#### ​gameStats​ :: ​GameStats​,

#### ​gameObjects​ :: ​GameObjects​ }

#### data​ ​GameStats​ = ​Stats​ { ​name​ :: ​String​,

#### ​score​ :: ​Int​ }

#### data​ ​GameObjects​ = ​GameObjs​ { ​asteroids​ :: [​Asteroid​],

#### ​missiles​ :: [​Missile​],

#### ​enemies​ :: [​Player​],

#### ​player​ :: ​Player​ }

### The player

#### The player type in our Asteroids game will be represented by an enumeration, in this

#### definition we define that the player can be either controlled by a Human or by the

#### Computer.

#### data​ ​PlayerType​ = ​Human​ | ​Computer

#### The actual player is modeled by the Spaceship data type that contains all the properties

#### to handle it.

#### data​ ​Spaceship​ = ​Ship​ { ​shipPosition​ :: ​Point​,

#### ​shipSpeed​ :: ​Vector​,

#### ​angle​ :: ​Float​,

#### ​boundingBox​ :: ​BoundingBox​,

#### ​bulletTimeout​ :: ​Int​,

#### ​playerType​ :: ​PlayerType​ }

#### As we can see by design the enemies and the player differ only in the player type. The

#### player has to defeat enemies (by launching missiles at them) and destroy asteroids while


#### avoiding being hit by the asteroids or enemy missiles to get a higher score. Missiles are

#### defined as follows:

#### data​ ​Missile​ = ​Msl​ { ​lifetime​ :: ​Int​,

#### ​position​ :: ​Point​,

#### ​boundingBox​ :: ​BoundingBox​,

#### ​speed​ :: ​Vector​ }

#### And the function that launches them is

#### -- Create a missile heading in the direction of the spaceship

#### shoot​ :: ​Spaceship​ -> ​Missile

### Player movement

#### Asteroids is a game that has independently moving particles. This means that the

#### movement is continuous and can vary in direction and speed for every object on screen.

#### The player can decide where to move on the screen.

#### The move function takes the position of an object and a speed vector, and returns the

#### new position of the object.

#### -- Moves an object in the direction of the specified vector

#### move​ :: ​Point​ -> ​Vector​ -> ​Point

### Player adversary

#### As mentioned before the difference between the player and the enemies dwells in the

#### PlayerType. The enemies movements are controlled by the AI function that will decide

#### the speed and the direction of the ship in order to obtain a clear shot on the player.


#### Another fundamental adversary of the user are the Asteroids that float around the screen

#### modeled using the following structure:

#### data​ ​Asteroid​ = ​As​ { ​size​ :: ​Int​,

#### ​position​ :: ​Point​,

#### ​boundingBox​ :: ​BoundingBox​,

#### ​speed​ :: ​Vector​ }

#### An important property of Asteroids and Spaceships is the BoundingBox. The bounding

#### box is a rectangular border that fully encapsulates an object. Ii is important because it

#### allows us to detect collisions between the asteroids and the player by simply checking

#### whether their bounding boxes intersect.

#### -- Checks whether two bounding boxes collide

#### checkCollision​ :: ​BoundingBox​ -> ​BoundingBox​ -> ​Bool


