module Gameplay where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Object
import Space
import System.Random
import Collision
import Motion
import Debug.Trace


isDown :: KeyState -> Bool
isDown Down = True
isDown _    = False

pelletTemplate :: Object
pelletTemplate = [(Circ ((0,0),10),red)]

data World = PelletWorld {
                    space       :: Space,
                    player      :: MovingObj,
                    bullets     :: [(MovingObj,Int)],
                    pellet      :: MovingObj,
                    score       :: Int,
                    keys :: ArrowState
                  } | Pause {
                    selection :: Int,
                    buttons:: [(String,World)],
                    backGround :: World -- world to be rendered as backGround
                  }

testPelletWorld :: Float -> Float -> IO World
testPelletWorld x y = do
              p <- getPellet x y
              return PelletWorld {space=kh (x/2) (y/2),player=testMovOb,pellet=p,bullets=[],score=0,keys = allOff}

getPellet :: Float -> Float -> IO MovingObj
getPellet sx sy = do
              g <- getStdGen
              let (x,g2) = random g :: (Float,StdGen)
              let (y,g3) = random g2 :: (Float,StdGen)
              setStdGen g3
              let loc = ((sx*(x-0.5),sy*(y-0.5)),(False,0::Float))
              return (pelletTemplate,loc,id)--id means the pelet isn't moving


--stepWorld
gameplay :: Float -> Float -> Float -> World -> IO World
gameplay x y f w@(PelletWorld _ _ _ _ _ _) = do
              let s = space w
              let spaceTick = tick s
              let tickedP = spaceTick (player w)
              p <- getPellet x y
              let pelletTaken = or [ (collides s (getLoc (pellet w))) o | o <- (getLoc tickedP):(map (getLoc.fst) (bullets w)) ]
              let oldKeys = keys w
              return PelletWorld {
                 space=s,
                 player = tickedP,
                 pellet = if pelletTaken then p else pellet w,
                 score = if pelletTaken then score w + 1 else score w,
                 bullets = [(tick s ob,l-1) | (ob,l) <- (bullets w) , l>0 , not $ collides s (getLoc (tickedP)) (getLoc (tick s ob))],
                 keys = keys w
               }
gameplay _ _ f w@(Pause _ _ _) = return w


--render
renderPelletWorld :: World -> IO Picture
renderPelletWorld w@(PelletWorld _ _ _ _ _ _) = do
                    let playerPic = spaceDraw (space w) (getLoc (player w))
                    let pelletPic = spaceDraw (space w) (getLoc (pellet w))
                    let bulletPic = Pictures $ map ((spaceDraw (space w)).getLoc.fst) (bullets w)
                    return $ Pictures [playerPic,bulletPic,pelletPic,renderScore w]
renderPelletWorld (Pause s os bg) = do
                    bgp <- renderPelletWorld bg
                    let oc = length os
                    let ops = [ (i == s,fromIntegral (25*oc-50*i) :: Float ,fst (os!!i)) | i <- [0..(oc -1)] ]
                    let menuPic = Pictures $ map renderMenuOption ops
                    return menuPic-- temporary code


renderMenuOption::(Bool,Float,String)->Picture
renderMenuOption (selected,height,text) = Pictures [box,translate] where
    col = if selected then red else black ::Color
    pic = color col $ Text text :: Picture
    scaled = Scale 0.2 0.2 pic :: Picture
    translate = Translate (-75) height scaled:: Picture
    box = color col $ line [(-75,height-10),(75,height-10),(75,height+30),(-75,height+30),(-75,height-10)] :: Picture



renderScore :: World -> Picture
renderScore p = let s = score p
                    pic = Text ("Score: " ++ (show s))
                    scaled = Scale 0.2 0.2 pic
                    translate = Translate (-75) 75 scaled
                in translate

--input
handlePelletWorld :: Event -> World -> IO World
handlePelletWorld (EventKey (SpecialKey KeyUp) Down _ _) (Pause x os bg) = return $ Pause (mod (x-1) (length os)) os bg
handlePelletWorld (EventKey (SpecialKey KeyDown) Down _ _) (Pause x os bg) = return $ Pause (mod (x+1) (length os)) os bg
handlePelletWorld (EventKey (SpecialKey KeyEnter) Down _ _) (Pause x os bg) = return $ snd (os !! x)
handlePelletWorld (EventKey (SpecialKey KeyEsc) Down _ _) w = return (Pause 0 [("Resume",w),("Save",w),("Quit",error "")] w) -- save resumes game this will be cooler when we have a XML system and can actualy save/load
handlePelletWorld (EventKey (SpecialKey KeySpace) Down _ _) w@(PelletWorld _ _ _ _ _ _) = do
  let (_,loc,_) = player w
  return PelletWorld {
    space = space w,
    player = player w,
    bullets = bullets w ++ [( (bulletOb,forward 60 loc,forward 20),90)],
    pellet = pellet w,
    score = score w,
    keys = keys w
  }
handlePelletWorld k w@(PelletWorld _ _ _ _ _ _) = do
                        let (pObj,pLoc,_) = (player w)
                        let oldKeys = keys w
                        let newKeys = keyPressMove k oldKeys
                        let newMot = keyToPol newKeys
                        return PelletWorld {
                          space = space w,
                          player = (pObj,pLoc,newMot),
                          bullets = bullets w,
                          pellet = pellet w,
                          score = score w,
                          keys = newKeys
                        }
handlePelletWorld x w = do
  --print x
  return w -- do nothing if key stroke not delt with



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
