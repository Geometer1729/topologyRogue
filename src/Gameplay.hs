module Gameplay where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Object
import Space
import System.Random
import Collision
import Motion


isDown :: KeyState -> Bool
isDown Down = True
isDown _    = False

pelletTemplate :: Object
pelletTemplate = [(Circ ((0,0),10),red)]

data World = PelletWorld {
                    space       :: Space,
                    player      :: MovingObj,
                    pellet      :: MovingObj,
                    score       :: Int,
                    keys :: ArrowState
                  } | Pause {
                    selection :: Int,
                    buttons:: [(String,World)],
                    backGround :: World -- world to be rendered as backGround
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
gameplay :: Float -> World -> IO World
gameplay f w@(PelletWorld _ _ _ _ _) = do
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
gameplay f w@(Pause _ _ _) = return w


--render
renderPelletWorld :: World -> IO Picture
renderPelletWorld w@(PelletWorld _ _ _ _ _) = do
                    let grid = renderGrid 10 500 500
                    let playerPic = spaceDraw (space w) (getLoc (player w))
                    let pelletPic = spaceDraw (space w) (getLoc (pellet w))
                    return $ Pictures [grid,playerPic,pelletPic,renderScore w]
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
handlePelletWorld k w@(PelletWorld _ _ _ _ _) = do
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
