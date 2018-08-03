module Definitions where

import Graphics.Gloss

--Entity types
data Entity = Player {ob ::MovingObj, cooldown :: Int}
              | PlayerProj {ob ::MovingObj, life :: Int}
              | Pellet { ob :: MovingObj }
              | Enemy { ob :: MovingObj , cooldown :: Int , targeted :: Bool}
              | EnemyProj { ob :: MovingObj , life :: Int}
data Outcome =  Entity EntityOutcome | World WorldOutcome
data WorldOutcome =  EndGame | IncScore Int | SetScore Int
type EntityOutcome = (Who,EntityEffect)
data Who = First | Second deriving Eq
data EntityEffect = Kill | Move Motion
type EffectedEntity = (Entity,[EntityOutcome])

--Object types
type LocalObj = (Object,Location)
type Object = [Part]
type Part = (Shape,Color)
data Shape = Pol Polygon | Circ Circle
type Polygon = [Point]
type Circle = (Point,Float)
type Location = (Point,Orientation)
type Orientation = (Bool,Float)

--Object objects
testob::Object
testob =[(Pol [(-20,-50),(30,0),(-20,50)],black) , (Pol [(-25,-30),(50,0),(-25,30)],red) , (Circ ((30,0),25) , blue ) , (Circ ((-20,50),10) , cyan )]
bulletOb::Object
bulletOb = [(Pol [(0,-10),(10,-8),(20,0),(10,8),(0,10)],black)]
enemyOb::Object
enemyOb = [(Pol [(-20,-20),(-20,20),(20,20),(20,-20)], black)]
laserOb::Object
laserOb = [(Pol [(-20,-2),(-20,2),(20,2),(20,-2)], red)]

--Space types
type Motion = (Point,Float)
type Space = [(Boundary,LocRule,MotRule)]
type Boundary = Location -> Bool
type LocRule = Location -> Location
type MotRule = Motion -> Motion
type MovingObj = (Object,Location,Motion)

--Space objects
tau::Float -- take that warren
tau=2*pi--2*3
testlocob :: LocalObj
testlocob = (testob,((0,0),(False,0::Float)))
localBullet:: LocalObj
localBullet = (bulletOb,((0,0),(False,0::Float)))

--Gameplay types
data World = PelletWorld {
                    space         :: Space,
                    entities      :: [Entity],
                    score         :: Int,
                    keys          :: Controls,
                    time          :: Float
                  } | Pause {
                    selection     :: Int,
                    buttons       :: [(String,World)],
                    backGround    :: World -- world to be rendered as backGround not yet implemented wouldn't be hard I just don't know how to do opacity in Gloss
                  }
data Controls = Controls{
                  kup :: Bool,
                  kdown :: Bool,
                  kleft :: Bool,
                  kright :: Bool,
                  spaceBar :: Bool
                  }
--Gameplay objects
allOff::Controls
allOff = Controls False False False False False
pelletTemplate :: Object
pelletTemplate = [(Circ ((0,0),10),red)]

--game Config
windowWidth :: Float
windowWidth = 1200
windowHeight :: Float
windowHeight = 700
fireCoolDown:: Int
fireCoolDown = 10
bulletSpeed:: Float
bulletSpeed = 30
bulletRange :: Int
bulletRange = 2700
bulletLife :: Int
bulletLife = round $ fromIntegral bulletRange / bulletSpeed
playerSpeed :: Float
playerSpeed = 10
playerTurnSpeed :: Float
playerTurnSpeed = 0.1
enemyCooldown::Int
enemyCooldown = 10 -- perfectly balanced as all things should be
laserSpeed :: Float
laserSpeed = 40
laserLife :: Int
laserLife = 15
