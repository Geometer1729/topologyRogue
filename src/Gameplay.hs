module Gameplay where

import Definitions
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Object
import Space
import System.Random
import Collision
import Debug.Trace
import Entity

isDown :: KeyState -> Bool
isDown Down = True
isDown _    = False




testPelletWorld :: Float -> Float -> IO World
testPelletWorld x y = do
              p <- getPellet x y
              return PelletWorld {space=kh (x/2) (y/2),entities = [Player testMovOb 0, p],score=0,keys = allOff, time=0}

getPellet :: Float -> Float -> IO Entity
getPellet sx sy = do
              g <- getStdGen
              let (x,g2) = random g :: (Float,StdGen)
              let (y,g3) = random g2 :: (Float,StdGen)
              setStdGen g3
              let loc = ((sx*(x-0.5),sy*(y-0.5)),(False,0::Float)) :: Location
              return $ Pellet (pelletTemplate,loc,((0,0),0)) --id means the pelet isn't moving


--stepWorld
gameplay :: Float -> Float -> Float -> World -> IO World
gameplay x y f w@PelletWorld{} = do
              let s = space w
              let es = entities w
              let es1 = [if isPlayer e then adjustPlayerMotion (keys w) e else e | e <- es] -- could potentialy adjust multiple players
              p <- getPellet x y
              let pelletTaken = not $ or $ map isPellet (es1) -- could also just be es
              let isFiring = spaceBar $ keys w
              let es2 = if isFiring then concat [tryToShoot e | e <- es1] else es1
              let (es3,worldUpdate) = handle s es2
              let es4 = concat $ map entityTick es3
              let es5 = map (motionTick s) es4
              let es6 = if pelletTaken then es5 ++ [p] else es5
              return $ worldUpdate $ PelletWorld {
                 space=s,
                 entities = es6,
                 score = score w,
                 keys = keys w,
                 time = f + time w
               }
gameplay _ _ f w@(Pause _ _ _) = return w


adjustPlayerMotion:: Controls -> Entity -> Entity
adjustPlayerMotion keys p = setOb ( setMot (keyToPol keys l) $ ob) p
  where
    ob@(_,l,_) = getOb p

--render
renderPelletWorld :: World -> IO Picture
renderPelletWorld w@PelletWorld{} = do
                    let entPic = map (entityDraw (space w)) (entities w)
                    return $ Pictures $ entPic ++ [renderScore w, renderTimer w]
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
handlePelletWorld k w@PelletWorld{} = do
                        let oldKeys = keys w
                        let newKeys = keyPressMove k oldKeys
                        return w { keys = newKeys }
handlePelletWorld x w = do
  --print x --uncoment for debug
  return w -- do nothing if key stroke not delt with above



keyPressMove :: Event -> Controls -> Controls
keyPressMove (EventKey k ks mods f@(x,y)) c = case k of
                                                    (SpecialKey KeyUp)    -> c {kup    = kd}
                                                    (SpecialKey KeyDown)  -> c {kdown  = kd}
                                                    (SpecialKey KeyLeft)  -> c {kleft  = kd}
                                                    (SpecialKey KeyRight) -> c {kright = kd}
                                                    (SpecialKey KeySpace) -> c {spaceBar = kd}
                                                    _ -> c
                                                    where
                                                      kd = isDown ks
keyPressMove _ l = l







keyToPol:: Controls -> Location -> Motion
keyToPol (Controls u d l r _) loc = trace (show (um,dm,lm,rm,foldl add still [um,dm,lm,rm])) $ foldl add still [um,dm,lm,rm]
  where
    um = if u then forward  10 loc else still
    dm = if d then backward 10 loc else still
    lm = if l then ccw 0.1 else still
    rm = if r then cw 0.1  else still

-- entity code that depends on world type

applyWorld::[WorldOutcome] -> World -> World
applyWorld [] = id
applyWorld (w:ws) = (applyWorldOutcome w) . (applyWorld ws)

applyWorldOutcome:: WorldOutcome -> World -> World
applyWorldOutcome EndGame  _ = error "Score Display menu thing not yet implemented"
applyWorldOutcome (SetScore n) (PelletWorld s e _ k t)  = (PelletWorld s e n k t)
applyWorldOutcome (IncScore n) (PelletWorld s e sc k t) = (PelletWorld s e (sc+n) k t)

handle:: Space -> [Entity] -> ([Entity],World->World)
handle s es = (newEnts, worldFunc)
  where
    (worldOuts,witheffects) = linkOutcomes s es
    newEnts = concat $ map applyEntityEffects witheffects :: [Entity]
    worldFunc = applyWorld worldOuts
