module Gameplay where

import Definitions
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Data.ViewPort
import Object
import Space
import System.Random
import Collision
import Entity
import Debug.Trace
import EntityGen

isDown :: KeyState -> Bool
isDown Down = True
isDown _    = False




testPelletWorld :: Space -> Float -> Float -> IO World
testPelletWorld s x y = do
              stars <- makeStars 50 x y
              let sr = [([1],Pellet (pelletTemplate,centered,still) , isPellet, const True, None),([1],Enemy  (enemyOb,centered,still) 10 10 True , isEnemy, const True, Max 10 0.1)]
              return PelletWorld {space= s,entities = stars ++ [Player testMovOb 10 0],spawnRules = sr ,score=0,keys = allOff, time=0, windowSize= (round windowWidth,round windowHeight)}


makeStars:: Int -> Float -> Float -> IO [Entity]
makeStars 0 _ _ = return []
makeStars n sx sy = do
  loc <- getRandomLoc sx sy
  more <- makeStars (n-1) sx sy
  return $ (Inert (starOb,loc,still)) : more

--stepWorld
gameplay :: Float -> Float -> Float -> World -> IO World
gameplay x y f w@PelletWorld{} = do
	putStr "gamePlay :"
	print $ windowSize w
	let es = entities w
	let playerAlive = or $ map isPlayer es
	if playerAlive then do
		let s = space w
		let es1 = [if isPlayer e then adjustPlayerMotion (keys w) e else e | e <- es] -- adjust player motion
		let (_,pLoc,_) = head $ map ob $ filter isPlayer es1 -- get player location
		let isFiring = fireing $ keys w
		let es2 = if isFiring then concat [tryToShoot e | e <- es1] else es1 -- create bullet if isFiring and cooldown is 0
		let (es3,worldUpdate) = handle s es2 -- process collision events
		let es4 = concat $ map entityTick es3 -- process autonomous entity actions (ie. enemy shooting)
		let (pv,_) = pLoc -- get player location vector
		let es5 = map (entityShift (recenter pv,0)) es4 -- adjust map to keep player somewhat centered
		let es6 = map (motionTick s) es5 -- process motion
		newEnts <- genSeveralEnts s es5 (spawnRules w)
		let es7 = es6++ newEnts
		worldUpdate $ w {entities = es7 , time = f + time w} -- set new entities and time then apply world effect actions
	else -- if player is dead just end the game
		applyWorldOutcome EndGame w
gameplay _ _ f w@Menu{} = return w




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
  print "render info:"
  print (windowSize w)
  let entPic = map (entityDraw (space w)) (entities w)
  let (screenX,screenY) = windowSize w
  let scale = max (fromIntegral screenX / windowWidth) (fromIntegral screenY / windowHeight)
  let newPort = viewPortInit{viewPortScale=scale}
  let entPic = map (entityDraw (space w)) (entities w)
  return $ applyViewPortToPicture newPort $ Pictures $ entPic ++ [renderScore w, renderTimer w, renderHp w]
renderPelletWorld (Menu s os bg _) = do
                    bgp <- renderPelletWorld bg
                    let oc = length os
                    let ops = [ (i == s,fromIntegral (25*oc-50*i) :: Float ,fst (os!!i)) | i <- [0..(oc -1)] ]
                    let menuPic = Pictures $ map renderMenuOption ops
                    return menuPic-- temporary code


renderMenuOption::(Bool,Float,String)->Picture
renderMenuOption (selected,height,text) = Pictures [box,translate] where
    col = if selected then red else white ::Color
    pic = color col $ Text text :: Picture
    scaled = Scale 0.2 0.2 pic :: Picture
    translate = Translate (-75) height scaled:: Picture
    box = color col $ line [(-75,height-10),(75,height-10),(75,height+30),(-75,height+30),(-75,height-10)] :: Picture


renderScore :: World -> Picture
renderScore p = let s = score p
                    pic = Text ("Score: " ++ (show s))
                    colored = color red pic
                    scaled = Scale 0.15 0.15 colored
                    translate = Translate (-windowWidth/2) (-windowHeight/2+40) scaled
                in translate
--if I do this one more time, I'm making a renderText function
renderTimer :: World -> Picture
renderTimer w = let t = time w
                    pic = Text ("Time: " ++ (show t))
                    colored = color red pic
                    scaled = Scale 0.15 0.15 colored
                    translate = Translate (-windowWidth/2) (-windowHeight/2) scaled
                in translate
renderHp :: World -> Picture
renderHp w = let    t = time w
                    players = filter isPlayer $ entities w
                    h = if length players == 0 then 0 else hp $ head players
                    pic = Text ("HP: " ++ (show h))
                    colored = color red pic
                    scaled = Scale 0.15 0.15 colored
                    translate = Translate (-windowWidth/2) (-windowHeight/2+20) scaled
                in translate


--input
handlePelletWorld :: Event -> World -> IO World
handlePelletWorld (EventKey (SpecialKey KeyUp) Down _ _) (Menu x os bg s) = return $ Menu (mod (x-1) (length os)) os bg s
handlePelletWorld (EventKey (SpecialKey KeyDown) Down _ _) (Menu x os bg s) = return $ Menu (mod (x+1) (length os)) os bg s
handlePelletWorld (EventKey (SpecialKey KeyEnter) Down _ _) (Menu x os bg s) = return $ snd (os !! x)
handlePelletWorld (EventKey (SpecialKey KeyEsc) Down _ _) w@PelletWorld{} = do
  restartWorld <- testPelletWorld (space w) windowWidth windowHeight
  return $ Menu 0 [("Resume",w),("Restart",restartWorld),("Quit",error "")] w (windowSize w) -- save resumes game this will be cooler when we have a XML system and can actualy save/load
handlePelletWorld (EventResize (x,y)) w = do
	putStr "event :"
	print $ windowSize w
	return w{windowSize=(x,y)}
handlePelletWorld k w@PelletWorld{} = do
                        print k -- debug code
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
  applyWorldOutcome o nw


applyWorldOutcome:: WorldOutcome -> World -> IO World
applyWorldOutcome _ m@Menu{} = return m -- prevents world actions being attempted on menu worlds
applyWorldOutcome EndGame  w = do
  newWorld <- testPelletWorld (space w) windowWidth windowHeight
  return $ Menu 0 [("PlayAgain",newWorld)] w (windowSize w)
applyWorldOutcome (SetScore n) w@PelletWorld{}  = return w{score = n}
applyWorldOutcome (IncScore n) w@PelletWorld{}  = return w{score =(score w +n)}

handle:: Space -> [Entity] -> ([Entity],World->IO World)
handle s es = (inerts++newEnts, worldFunc)
  where
    inerts = filter isInert es
    nonInerts = filter (not . isInert) es
    (worldOuts,witheffects) = linkOutcomes s nonInerts
    newEnts = concat $ map applyEntityEffects witheffects :: [Entity]
    worldFunc = applyWorld worldOuts
