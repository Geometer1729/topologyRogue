{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}

module Entity where

import Definitions
import Space
import Object
import Collision
import Graphics.Gloss



getOb::Entity -> MovingObj
getOb (Player x _) = x
getOb (PlayerProj x _) = x
getOb (Pellet x) = x

entityDraw::Space -> Entity -> Picture
entityDraw s = (spaceDraw s) . getLoc . getOb

setOb::MovingObj -> Entity -> Entity
setOb m (Player _ t) = Player m t
setOb m (PlayerProj _ l) = PlayerProj m l
setOb m (Pellet _) = Pellet m

isPlayer::Entity -> Bool
isPlayer (Player _ _) = True
isPlayer _ = False

isPlayerProj::Entity -> Bool
isPlayerProj (PlayerProj _ _) = True
isPlayerProj _ = False

isPellet::Entity -> Bool
isPellet (Pellet _) = True
isPellet _ = False


entitiesCollide::Space -> Entity -> Entity -> Bool
entitiesCollide s a b = collides s (newLoc a) (newLoc b)
  where
    newLoc :: Entity -> LocalObj
    newLoc = getLoc . (tick s) . getOb

flipWho::Who->Who
flipWho First = Second
flipWho Second = First

flipEntityOutcome:: EntityOutcome -> EntityOutcome
flipEntityOutcome (x,y) = (flipWho x,y)

flipOutcome :: Outcome -> Outcome
flipOutcome (World x) = World x
flipOutcome (Entity x) = Entity $ flipEntityOutcome x

collisionHandle::Entity -> Entity -> [Outcome]
collisionHandle a b = (halfCollisionHandle a b) ++ (halfCollisionHandle b a)

halfCollisionHandle:: Entity -> Entity -> [Outcome]
halfCollisionHandle (Player _ _)     (PlayerProj _ _) = [ Entity (Second,Kill)]
halfCollisionHandle (Player _ _)     (Pellet _)       = [Entity (Second,Kill), World $ IncScore 1]
halfCollisionHandle (PlayerProj _ _) (Pellet _)       = [Entity (Second,Kill), World $ IncScore 1]
halfCollisionHandle _ _ = [] -- unspecified behavior do nothing

findCollisions::Space -> [Entity] -> [(Int,Int)]
findCollisions s es = filter check (triangleList $ length es -1)
  where
    check:: (Int,Int) -> Bool
    check (n,m) = entitiesCollide s (es!!n) (es!!m)

triangleList::Int -> [(Int,Int)]
triangleList n = concat [upto m | m <- [0..n] ]
  where
    upto:: Int -> [(Int,Int)]
    upto n = [(m,n) | m <- [0..n]]

isWorld::Outcome -> Bool
isWorld (World _) = True
isWorld _ = False

isEntity:: Outcome -> Bool
isEntity (Entity _) = True
isEntity _ = False

linkOutcomes::Space -> [Entity] -> ([WorldOutcome],[(Entity,[EntityEffect])])
linkOutcomes s es = (worldEffects,linkedOutcomes)
  where
    (worldEffects,indexedOutcomes) = arrangeOutcomes s es :: ([WorldOutcome],([(Int,EntityEffect)]))
    linkedOutcomes = [(es!!n, [o | (m,o) <- indexedOutcomes , m == n] ) | n <- [0..length es -1]] :: [(Entity,[EntityEffect])]

arrangeOutcomes::Space -> [Entity] -> ([WorldOutcome],[(Int,EntityEffect)])
arrangeOutcomes s es = (worldEffects,assigned)
  where
    collisionPairs = findCollisions s es :: [(Int,Int)]
    rawOutcomes = [(n,m, collisionHandle (es!!n) (es!!m)) | (n,m) <- collisionPairs ] :: [(Int,Int,[Outcome])]
    separated = separate rawOutcomes
    worldEffects = map getWorld $ filter isWorld $ concat [o | (_,_,o) <- rawOutcomes] ::[WorldOutcome]
    entityEffects = [ (n,m, getEntity o) | (n,m,o) <- separated , isEntity o] :: [(Int,Int,EntityOutcome)]
    assigned = map assign entityEffects :: [(Int,EntityEffect)]

separate::[(a,b,[c])] -> [(a,b,c)]
separate [] = []
separate ((y1,y2,ys):xs) = [(y1,y2,y) | y <-ys ] ++ separate xs

assign::(Int,Int,EntityOutcome) -> (Int,EntityEffect)
assign (n,m,(who,o)) = if who == First then (n,o) else (m,o)

getWorld::Outcome -> WorldOutcome
getWorld (World x) = x
getWorld _ = error "Not a World Outcome"

getEntity:: Outcome -> EntityOutcome
getEntity (Entity x) = x
getEntity _ = error "Not an Entity Outcome"


applyEntityEffects::(Entity,[EntityEffect]) -> [Entity]
applyEntityEffects (e,[]) = [e]
applyEntityEffects (e,(f:fs)) =  concat [ applyEntityEffects (r,fs) | r <- (applyEntityEffect f e)]

applyEntityEffect::EntityEffect -> Entity -> [Entity] -- returning a list allows killing or firing
applyEntityEffect Kill _ = []
applyEntityEffect (Move m) e = [setOb  (setMot m $ getOb e) e]

entityTick::Entity -> [Entity]
entityTick (Player o h) = [Player o (max 0 (h-1))] -- decriment fire cooldown
entityTick (PlayerProj _ 0) = [] -- kill if lifespan is 0
entityTick (PlayerProj o l) = [PlayerProj o (max 0 (l-1))] -- decriment life span
entityTick x = [x] -- tick is id for unspecified

motionTick::Space -> Entity -> Entity
motionTick s e = setOb moved e
  where
    moved = tick s (getOb e) :: MovingObj

tryToShoot::Entity -> [Entity]
tryToShoot (Player o@(_,l,_) 0) = [Player o fireCoolDown , PlayerProj (bulletOb, app ( forward 60 l)$ l,forward bulletSpeed l) bulletLife ]
tryToShoot e@(Player _ _) = [e]
tryToShoot x = [x]
