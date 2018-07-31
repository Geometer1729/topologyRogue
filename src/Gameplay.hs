module Gameplay where
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Object
import           Space
import           System.Random
import Collision
import Motion
import SpaceRenderer

pelletTemplate :: Object
pelletTemplate = [(Circ ((0,0),10),red)]

data PelletWorld = PelletWorld {
                    space       :: Space,
                    player      :: MovingObj,
                    pellet      :: MovingObj,
                    score       :: Int,
                    keys :: ArrowState
                  }

testPelletWorld = do
              p <- getPellet 500
              return PelletWorld {space=kh 250 250,player=testMovOb,pellet=p,score=0,keys = allOff}

getPellet :: Float -> IO MovingObj
getPellet size = do
              g <- getStdGen
              let (x,g2) = random g :: (Float,StdGen)
              let (y,g3) = random g2 :: (Float,StdGen)
              setStdGen g3
              let loc = ((size*(x-0.5),size*(y-0.5)),(False,0::Float))
              return (pelletTemplate,loc,id)--id means the pelet isn't moving


--stepWorld
gameplay :: Float -> PelletWorld -> IO PelletWorld
gameplay f w = do
              let s = space w
              let spaceTick = tick s
              let tickedP = spaceTick (player w)
              p <- getPellet 500
              let pelletTaken = collides s (getLoc tickedP) (getLoc (pellet w))
              let oldKeys = keys w
              return PelletWorld {
                 space=s,
                 player = tickedP,
                 pellet = if pelletTaken then p else pellet w,
                 score = if pelletTaken then score w + 1 else score w,
                 keys = oldKeys
               }

--render
renderPelletWorld :: PelletWorld -> IO Picture
renderPelletWorld w = do
                    let grid = renderGrid 10 500 500
                    let playerPic = spaceDraw (space w) (getLoc (player w))
                    let pelletPic = spaceDraw (space w) (getLoc (pellet w))
                    return $ Pictures [grid,playerPic,pelletPic,renderScore w]

renderScore :: PelletWorld -> Picture
renderScore p = let s = score p
                    pic = Text ("Score: " ++ (show s))
                    scaled = Scale 0.2 0.2 pic
                    translate = Translate (-75) 75 scaled
                in translate

--input
handlePelletWorld :: Event -> PelletWorld -> IO PelletWorld
handlePelletWorld k w = do
                        let (pObj,pLoc,_) = (player w)
                        let oldKeys = keys w
                        let newKeys = keyPressMove k oldKeys
                        let newMot = keyToPol newKeys
                        return PelletWorld {
                          space = space w,
                          player = (pObj,pLoc,newMot),
                          pellet = pellet w,
                          score = score w,
                          keys = newKeys
                        }



keyPressMove :: Event -> ArrowState -> ArrowState
keyPressMove (EventKey k ks mods f@(x,y)) as = case k of
                                                    (SpecialKey KeyUp) -> as {kup = kd}
                                                    (SpecialKey KeyDown) -> as {kdown = kd}
                                                    (SpecialKey KeyLeft) -> as {kleft= kd}
                                                    (SpecialKey KeyRight) -> as {kright = kd}
                                                    _ -> as
                                                    where
                                                      kd = isDown ks
keyPressMove _ l = l


data ArrowState = ArrowState{
kup :: Bool,
kdown :: Bool,
kleft :: Bool,
kright :: Bool
}

allOff::ArrowState
allOff = ArrowState False False False False

keyToCart:: ArrowState -> Motion
keyToCart (ArrowState u d l r) = um.dm.lm.rm
  where
    um = if u then mup 10 else id
    dm = if d then mdown 10 else id
    lm = if l then mleft 10 else id
    rm = if r then mright 10 else id

keyToPol:: ArrowState -> Motion
keyToPol (ArrowState u d l r) = um.dm.lm.rm
  where
    um = if u then forward 10 else id
    dm = if d then backward 10 else id
    lm = if l then ccw 0.1 else id
    rm = if r then cw 0.1 else id

ccw::Float -> Motion
ccw dtheta (pt,(f,theta)) = (pt,(f,theta+(maybeNeg f dtheta)))

cw::Float -> Motion
cw x = ccw (-1*x)

forward::Float -> Motion
forward x l@(pt,(f,theta)) = comp (vecToLoc (cos thetar * x,sin thetar * x)) l
  where
    thetar = theta + pi /2

backward::Float -> Motion
backward x = forward (-1*x)

maybeNeg::Bool->Float->Float
maybeNeg False = id
maybeNeg True = (*) (-1)
