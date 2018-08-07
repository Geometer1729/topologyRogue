module Definitions where

import Graphics.Gloss

--Entity types
data Entity = Player       {ob :: MovingObj, hp :: Int , cooldown :: Int}
              | PlayerProj {ob :: MovingObj, life :: Int}
              | Enemy      {ob :: MovingObj, hp :: Int , cooldown :: Int , targeted :: Bool}
              | EnemyProj  {ob :: MovingObj, life :: Int}
              | Pellet     {ob :: MovingObj}
              | Inert      {ob :: MovingObj} deriving Show
data Outcome =  Entity EntityOutcome | World WorldOutcome
data WorldOutcome =  EndGame | IncScore Int | SetScore Int deriving Show
type EntityOutcome = (Who,EntityEffect)
data Who = First | Second deriving Eq
data EntityEffect = Damage Int | Kill | Move Motion
type EffectedEntity = (Entity,[EntityOutcome])

--Object types
type LocalObj = (Object,Location)
type Object = [Part]
type Part = (Shape,Color)
data Shape = Pol Polygon | Circ Circle deriving Show
type Polygon = [Point]
type Circle = (Point,Float)
type Location = (Point,Orientation)
type Orientation = (Bool,Float)
type EntityGenRule = ([Float],Entity,Entity -> Bool ,Location->Bool,MotionRange) -- [ [probability of spawn for each number], count filter, what to spawn, location filter) ]
data MotionRange = None | Max Float Float -- displacement rotation

metal::Color
metal = makeColor 0.2 0.2 0.2 1
centered::Location
centered = ((0,0),(False,0))
still::Motion
still = ((0,0),0)

--Object objects
testob::Object
testob =[(Pol [(-20,-50),(30,0),(-20,50)],metal) , (Pol [(-25,-30),(50,0),(-25,30)],red) ]
bulletOb::Object
bulletOb = [(Pol [(0,-10),(10,-8),(20,0),(10,8),(0,10)],metal)]
enemyOb::Object
enemyOb = [(Pol [(-20,-20),(-20,20),(20,20),(20,-20)], metal)]
laserOb::Object
laserOb = [(Pol [(-20,-2),(-20,2),(20,2),(20,-2)], red)]
starOb ::Object
starOb = [(Circ ((0,0),3) , white) ]

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
                    spawnRules    :: [EntityGenRule],
                    score         :: Int,
                    keys          :: Controls,
                    time          :: Float
                  } | Menu {
                    selection     :: Int,
                    buttons       :: [(String,World)],
                    backGround    :: World -- world to be rendered as backGround not yet implemented wouldn't be hard I just don't know how to do opacity in Gloss
                  }
instance Show World where
  show PelletWorld{entities = e, score = s} = concat [show e , "Score" , show s]
  show Menu{buttons = b} = concat $ map (fst) b
data Controls = Controls{
                  kup :: Bool,
                  kdown :: Bool,
                  kleft :: Bool,
                  kright :: Bool,
                  fireing :: Bool,
                  cursor :: Point
                  }
--Gameplay objects
allOff::Controls
allOff = Controls False False False False False (10,0)
pelletTemplate :: Object
pelletTemplate = [(Circ ((0,0),10),red)]

--game Config
windowWidth :: Float
windowWidth = 1600
windowHeight :: Float
windowHeight = 900
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
laserSpeed = 30
laserLife :: Int
laserLife = 15

killL::Outcome
killL = Entity (First,Kill)
killR::Outcome
killR = Entity (Second,Kill)
gameOver::Outcome
gameOver = World EndGame
-- technicaly functions but they are only used as shorthand
damL::Int -> Outcome
damL n = Entity (First,Damage n)
damR :: Int -> Outcome
damR n = Entity (Second,Damage n)
addPoint::Int->Outcome
addPoint n = World $ IncScore n
