module Gameplay where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Object
import Space
import System.Random
import Collision
import Debug.Trace


isDown :: KeyState -> Bool
isDown Down = True
isDown _    = False

pelletTemplate :: Object
pelletTemplate = [(Circ ((0,0),10),red)]

data World = PelletWorld {
                    space         :: Space,
                    player        :: MovingObj,
                    bullets       :: [(MovingObj,Int)],
                    pellet        :: MovingObj,
                    score         :: Int,
                    keys          :: ArrowState,
                    time          :: Float
                  } | Pause {
                    selection     :: Int,
                    buttons       :: [(String,World)],
                    backGround    :: World -- world to be rendered as backGround
                  }

testPelletWorld :: Float -> Float -> IO World
testPelletWorld x y = do
              p <- getPellet x y
              return PelletWorld {space=kh (x/2) (y/2),player=testMovOb,pellet=p,bullets=[],score=0,keys = allOff, time=0}

getPellet :: Float -> Float -> IO MovingObj
getPellet sx sy = do
              g <- getStdGen
              let (x,g2) = random g :: (Float,StdGen)
              let (y,g3) = random g2 :: (Float,StdGen)
              setStdGen g3
              let loc = ((sx*(x-0.5),sy*(y-0.5)),(False,0::Float)) :: Location
              return (pelletTemplate,loc,((0,0),0)) --id means the pelet isn't moving


--stepWorld
gameplay :: Float -> Float -> Float -> World -> IO World
gameplay x y f w@(PelletWorld _ _ _ _ _ _ _) = do
              let s = space w
              let (po,pl,pm) = player w
              let tickedP = (po, spaceReduce s (app pm pl) , keyToPol (keys w) (spaceReduce s (app pm pl))) -- can't be standard tick as motion needs to be re interpreted
              p <- getPellet x y
              let pelletTaken = or [ (collides s (getLoc (pellet w))) o | o <- (getLoc tickedP):(map (getLoc.fst) (bullets w)) ]
              let oldKeys = keys w
              return PelletWorld {
                 space=s,
                 player = tickedP,
                 pellet = if pelletTaken then p else pellet w,
                 score = if pelletTaken then score w + 1 else score w,
                 bullets = [(tick s ob,l-1) | (ob,l) <- (bullets w) , l>0 , not $ collides s (getLoc (tickedP)) (getLoc (tick s ob))],
                 keys = keys w,
                 time = f + time w
               }
gameplay _ _ f w@(Pause _ _ _) = return w


--render
renderPelletWorld :: World -> IO Picture
renderPelletWorld w@(PelletWorld _ _ _ _ _ _ _) = do
                    let playerPic = spaceDraw (space w) (getLoc (player w))
                    let pelletPic = spaceDraw (space w) (getLoc (pellet w))
                    let bulletPic = Pictures $ map ((spaceDraw (space w)).getLoc.fst) (bullets w)
                    return $ Pictures [playerPic,bulletPic,pelletPic,renderScore w, renderTimer w]
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
--if I do this one more time, I'm making a renderText function
renderTimer :: World -> Picture
renderTimer w = let t = time w
                    pic = Text ("Time: " ++ (show t))
                    scaled = Scale 0.2 0.2 pic
                    translate = Translate (-75) 125 scaled
                in translate

--input
handlePelletWorld :: Event -> World -> IO World
handlePelletWorld (EventKey (SpecialKey KeyUp) Down _ _) (Pause x os bg) = return $ Pause (mod (x-1) (length os)) os bg
handlePelletWorld (EventKey (SpecialKey KeyDown) Down _ _) (Pause x os bg) = return $ Pause (mod (x+1) (length os)) os bg
handlePelletWorld (EventKey (SpecialKey KeyEnter) Down _ _) (Pause x os bg) = return $ snd (os !! x)
handlePelletWorld (EventKey (SpecialKey KeyEsc) Down _ _) w = return (Pause 0 [("Resume",w),("Save",w),("Quit",error "")] w) -- save resumes game this will be cooler when we have a XML system and can actualy save/load
handlePelletWorld (EventKey (SpecialKey KeySpace) Down _ _) w@(PelletWorld _ _ _ _ _ _ _) = do
  let (_,loc,_) = player w
  return PelletWorld {
    space = space w,
    player = player w,
    bullets = bullets w ++ [( (bulletOb, app (forward 60 loc)$ loc,forward 20 loc),90)],
    pellet = pellet w,
    score = score w,
    keys = keys w,
    time = time w
  }
handlePelletWorld k w@(PelletWorld _ _ _ _ _ _ _) = do
                        let (pObj,pLoc,_) = (player w)
                        let oldKeys = keys w
                        let newKeys = keyPressMove k oldKeys
                        let newMot = traceShowId $ keyToPol newKeys pLoc
                        return PelletWorld {
                          space = space w,
                          player = (pObj,pLoc,newMot),
                          bullets = bullets w,
                          pellet = pellet w,
                          score = score w,
                          keys = newKeys,
                          time = time w
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


keyToPol:: ArrowState -> Location -> Motion
keyToPol (ArrowState u d l r) loc = trace (show (um,dm,lm,rm,foldl add still [um,dm,lm,rm])) $ foldl add still [um,dm,lm,rm]
  where
    um = if u then forward  10 loc else still
    dm = if d then backward 10 loc else still
    lm = if l then ccw 0.1 else still
    rm = if r then cw 0.1  else still

ccw::Float -> Motion
ccw dtheta = ((0,0),dtheta)

cw::Float ->  Motion
cw x = ccw (-1*x)

forward::Float -> Location -> Motion
forward x (_,(f,theta)) = traceShowId $ ((cos theta * x,sin theta * x),0)

backward::Float -> Location -> Motion
backward x = forward (-1*x)
