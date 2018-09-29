{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}

module Engine.Entity where
import Engine.Definitions
import Engine.Space
import Engine.Object
import Engine.Collision
import Graphics.Gloss

-- non general code

isPlayer::Entity -> Bool
isPlayer Player{} = True
isPlayer _ = False

isPlayerProj::Entity -> Bool
isPlayerProj PlayerProj{} = True
isPlayerProj _ = False

isPellet::Entity -> Bool
isPellet Pellet{} = True
isPellet _ = False

isEnemy::Entity -> Bool
isEnemy Enemy{} = True
isEnemy _ = False

isEnemyProj::Entity -> Bool
isEnemyProj EnemyProj{} = True
isEnemyProj _ = False

isInert::Entity -> Bool
isInert Inert{} = True
isInert _ = False

halfCollisionHandle:: Entity -> Entity -> [Outcome]
halfCollisionHandle Player{}     PlayerProj{} = [killR]
halfCollisionHandle Player{}     Pellet{}     = [killR, addPoint 1]
halfCollisionHandle PlayerProj{} Pellet{}     = [killR, addPoint 1]
halfCollisionHandle PlayerProj{} PlayerProj{} = [killL]  -- still kills both
halfCollisionHandle Player{}     EnemyProj{}  = [damL 1, killR , addPoint (-1)]
halfCollisionHandle PlayerProj{} Enemy{}      = [killR, addPoint 10]
halfCollisionHandle EnemyProj{}  Pellet{}     = [killR, addPoint (-1)]
halfCollisionHandle Player{}     Enemy{}      = [killL , gameOver]
halfCollisionHandle _ _ = [] -- unspecified behavior do nothing

entityTick::Entity -> [Entity]
entityTick p@Player{cooldown = c} = [p { cooldown = max 0 (c-1)}] -- decriment fire cooldown
entityTick PlayerProj{life = 0} = [] -- kill if lifespan is 0
entityTick p@PlayerProj{life = l} = [p { life = max 0 (l-1)}] -- decriment life span
entityTick e@Enemy{targeted = True , cooldown = 0 ,ob = (_,l,_) } = [e{cooldown = enemyCooldown},EnemyProj (laserOb, app ( forward 15 l)$ l,forward laserSpeed l) laserLife]
entityTick e@Enemy{cooldown = c} = [e { cooldown = max 0 (c-1)}]
entityTick EnemyProj{life = 0} = [] -- kill if lifespan is 0
entityTick p@EnemyProj{life = l} = [p { life = max 0 (l-1)}] -- decriment life span
entityTick x = [x] -- tick is id for unspecified

tryToShoot::Entity -> [Entity] -- only for player other entitys must use entityTick to shoot
tryToShoot p@Player{ob = o@(_,l,_), cooldown = 0} = [p {cooldown = fireCoolDown} , PlayerProj (bulletOb, app ( forward 60 l)$ l,forward bulletSpeed l) bulletLife ]
tryToShoot e@Player{} = [e]
tryToShoot x = [x]

-- general code

entityDraw::Space -> Entity -> Picture
entityDraw s = (spaceDraw s) . getLoc . ob

entitiesCollide::Space -> Entity -> Entity -> Bool
entitiesCollide s a b = collides s (newLoc a) (newLoc b)
  where
    newLoc :: Entity -> LocalObj
    newLoc = getLoc . (tick s) . ob

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

findCollisions::Space -> [Entity] -> [(Int,Int)]
findCollisions s es = filter check (triangleList $ length es -1)
  where
    check:: (Int,Int) -> Bool
    check (n,m) = entitiesCollide s (es!!n) (es!!m)

triangleList::Int -> [(Int,Int)]
triangleList n = concat [upto m | m <- [0..n] ]
  where
    upto:: Int -> [(Int,Int)]
    upto n = [(m,n) | m <- [0..n-1]]

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
applyEntityEffect (Move m) e = [entityShift m e]
applyEntityEffect (Damage n) e
  | hp e > n = [e{hp = hp e - n}]
  | otherwise = []

entityShift::Motion -> Entity -> Entity
entityShift mo e = e { ob = moved}
  where
    (o,l,m) = ob e
    moved = (o,app mo l,m)

motionTick::Space -> Entity -> Entity
motionTick s e = e {ob = moved}
  where
    moved = tick s (ob e) :: MovingObj
