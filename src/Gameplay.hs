module Gameplay where

import Definitions
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Object
import Space
import System.Random
import Collision
import Entity
import Debug.Trace

isDown :: KeyState -> Bool
isDown Down = True
isDown _    = False




testPelletWorld :: Space -> Float -> Float -> IO World
testPelletWorld s x y = do
              p <- getPellet x y
              e <- getEnemy s ((0,0),(False,0)) x y
              return PelletWorld {space= s,entities = [Player testMovOb 0, p ,e] ,score=0,keys = allOff, time=0}

getPellet :: Float -> Float -> IO Entity
getPellet sx sy = do
              g <- getStdGen
              let (x,g2) = random g :: (Float,StdGen)
              let (y,g3) = random g2 :: (Float,StdGen)
              setStdGen g3
              let loc = ((sx*(x-0.5),sy*(y-0.5)),(False,0::Float)) :: Location
              return $ Pellet (pelletTemplate,loc,((0,0),0)) --id means the pelet isn't moving


getEnemy :: Space -> Location -> Float -> Float -> IO Entity
getEnemy s playerLoc sx sy = do
              loc <- getFilteredRandomLoc sx sy (\l -> spaceNorm s playerLoc l > 200)
              g <- getStdGen
              let (dx,g2)      = random g :: (Float,StdGen)
              let (dy,g3)      = random g2 :: (Float,StdGen)
              let (dtheta,g4)  = random g3 :: (Float,StdGen)
              setStdGen g4
              return $ Enemy (enemyOb,loc, ((10*(dx-0.5),10*(dy-0.5)),0.2*(dtheta-0.5))) 10 True --id means the pelet isn't moving

getFilteredRandomLoc :: Float -> Float -> (Location -> Bool) -> IO Location
getFilteredRandomLoc sx sy test = do
  try <- getRandomLoc sx sy
  if test try then return try else getFilteredRandomLoc sx sy test

getRandomLoc::Float -> Float -> IO Location
getRandomLoc sx sy = do
  g <- getStdGen
  let (x,g2)       = random g  :: (Float,StdGen)
  let (y,g3)       = random g2 :: (Float,StdGen)
  let (theta,g4)   = random g3 :: (Float,StdGen)
  let (fliped,g5)  = random g4 :: (Bool,StdGen)
  setStdGen g5
  return ((sx*(x-0.5),sy*(y-0.5)),(fliped ,theta*tau))

--stepWorld
gameplay :: Float -> Float -> Float -> World -> IO World
gameplay x y f w@PelletWorld{} = do
              let s = space w
              let es = entities w
              let es1 = [if isPlayer e then adjustPlayerMotion (keys w) e else e | e <- es] -- could potentialy adjust multiple players
              let (_,pLoc,_) = head $ map ob $ filter isPlayer es1
              p <- getPellet x y
              e <- getEnemy s pLoc x y
              let pelletTaken = not $ or $ map isPellet (es1) -- could also just be es
              let enemyKilled = not $ or $ map isEnemy (es1)
              let isFiring = fireing $ keys w
              let es2 = if isFiring then concat [tryToShoot e | e <- es1] else es1
              let (es3,worldUpdate) = handle s es2
              let es4 = concat $ map entityTick es3
              let es5 = map (motionTick s) es4
              let es6 = if pelletTaken then es5 ++ [p] else es5
              let es7 = if enemyKilled then es6 ++ [] else es6
              let (pv,_) = pLoc
              let es8 = map (entityShift (recenter pv,0)) es7
              nextworld <- worldUpdate $ w {entities = es8 , time = f + time w}
              return nextworld
gameplay _ _ f w@(Menu _ _ _) = return w

recenter::Point -> Point
recenter p@(x,y)
  | len <= 10 = (x,y)
  | len > 10 = ptSub (ptScale 50 unit) p
  where
    len = sqrt $ x^2 + y^2
    unit = ptScale (1/len) p

ptScale::Float -> Point -> Point
ptScale r (x,y) = (r*x,r*y)


adjustPlayerMotion:: Controls -> Entity -> Entity
adjustPlayerMotion keys p = p { ob = ( setMot (keyToPol keys l) $ o) }
  where
    o@(_,l,_) = ob p

--render
renderPelletWorld :: World -> IO Picture
renderPelletWorld w@PelletWorld{} = do
                    let entPic = map (entityDraw (space w)) (entities w)
                    return $ Pictures $ entPic ++ [renderScore w, renderTimer w]
renderPelletWorld (Menu s os bg) = do
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
handlePelletWorld (EventKey (SpecialKey KeyUp) Down _ _) (Menu x os bg) = return $ Menu (mod (x-1) (length os)) os bg
handlePelletWorld (EventKey (SpecialKey KeyDown) Down _ _) (Menu x os bg) = return $ Menu (mod (x+1) (length os)) os bg
handlePelletWorld (EventKey (SpecialKey KeyEnter) Down _ _) (Menu x os bg) = return $ snd (os !! x)
handlePelletWorld (EventKey (SpecialKey KeyEsc) Down _ _) w@PelletWorld{} = do
  restartWorld <- testPelletWorld (space w) windowWidth windowHeight
  return (Menu 0 [("Resume",w),("Restart",restartWorld),("Quit",error "")] w) -- save resumes game this will be cooler when we have a XML system and can actualy save/load
handlePelletWorld k w@PelletWorld{} = do
                        let oldKeys = keys w
                        let newKeys = keyPressMove k oldKeys
                        return w { keys = newKeys }
handlePelletWorld x w = return w



keyPressMove :: Event -> Controls -> Controls
keyPressMove (EventKey k ks mods p) c = case k of
                                                    (Char 'w')    -> c {kup  = kd,cursor = p}
                                                    (Char 's')  -> c {kdown  = kd,cursor = p}
                                                    (Char 'a')  -> c {kleft  = kd,cursor = p}
                                                    (Char 'd') -> c {kright  = kd,cursor = p}
                                                    (MouseButton LeftButton) -> c {fireing = kd , cursor = p}
                                                    _ -> c
                                                    where
                                                  kd = isDown ks
keyPressMove (EventMotion p) l = l{cursor = p}
keyPressMove x w = traceShow x w

keyToPol:: Controls -> Location -> Motion
keyToPol (Controls u d l r _ c) loc@(pv,(fliped,theta)) = foldl add still [um,dm,lm,rm,rot]
  where
    um = if u then forward  playerSpeed loc else still
    dm = if d then backward playerSpeed loc else still
    lm = if l then left     playerSpeed loc else still
    rm = if r then right    playerSpeed loc else still
    rot = if fliped then cw (cursorTheta - theta) else ccw (cursorTheta - theta)
    cursorTheta = getTheta $ ptSub c pv

getTheta::Point -> Float
getTheta (x,y)
  | x == 0 = if y >0 then tau/4 else 3*tau/4
  | x > 0  = atan (y/x)
  | x < 0  = tau/2 + atan (y/x)


-- entity code that depends on world type

applyWorld::[WorldOutcome] -> World -> IO World
applyWorld [] w = return w
applyWorld (o:os) w = do
  nw <- applyWorld os w
  fw <- applyWorldOutcome o nw
  return fw

applyWorldOutcome:: WorldOutcome -> World -> IO World
applyWorldOutcome _ m@Menu{} = return m -- prevents world actions being attempted on menu worlds
applyWorldOutcome EndGame  w = do
  newWorld <- testPelletWorld (space w) windowWidth windowHeight
  return $ Menu 0 [("PlayAgain",newWorld)] w
applyWorldOutcome (SetScore n) (PelletWorld s e _ k t)  = return $ (PelletWorld s e n k t)
applyWorldOutcome (IncScore n) (PelletWorld s e sc k t) = return $ (PelletWorld s e (sc+n) k t)


handle:: Space -> [Entity] -> ([Entity],World->IO World)
handle s es = (newEnts, worldFunc)
  where
    (worldOuts,witheffects) = linkOutcomes s es
    newEnts = concat $ map applyEntityEffects witheffects :: [Entity]
    worldFunc = applyWorld worldOuts
