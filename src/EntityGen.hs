module EntityGen where
import Definitions
import Entity
import Space
import System.Random

genSeveralEnts:: Space -> [Entity] -> [EntityGenRule] -> IO [Entity]
genSeveralEnts s es rs = fmap concat $ sequence $ map (genEnt s es) rs

genEnt:: Space -> [Entity] -> EntityGenRule -> IO [Entity]
genEnt s es (ps,e,ef,lf,mr) = do
  let current = count es ef
  if current < length ps then do
    m <-possibly (ps!!current)
    if m then do
      loc <- getFiltered (getRandomLoc windowWidth windowHeight) lf
      mot <- getRandomMotion mr
      let (o,_,_) = ob e
      let new = (o,loc,mot)
      return [e{ob = new}]
    else return []
  else return []

getRandomMotion::MotionRange -> IO Motion
getRandomMotion None = return still
getRandomMotion (Max m r) = do
  g <- getStdGen
  let (dx,g2)       = randomR (-m,m)  g  :: (Float,StdGen)
  let (dy,g3)       = randomR (-m,m)  g2 :: (Float,StdGen)
  let (theta,g4)    = randomR (-r,r) g3 :: (Float,StdGen)
  setStdGen g4
  return ((dx,dy),theta)

possibly:: Float -> IO Bool
possibly p = do
  g <- getStdGen
  let (r,g2) = random g
  setStdGen g2
  return $ r < p

count::[a]->(a->Bool)->Int
count xs f = length $ filter f xs


getFiltered:: IO a -> (a -> Bool) -> IO a
getFiltered meth filt = do
  try <- meth -- try meth kids!
  if filt try then return try else getFiltered meth filt

getRandomLoc::Float -> Float -> IO Location
getRandomLoc sx sy = do
  g <- getStdGen
  let (x,g2)       = randomR (-sx,sx) g  :: (Float,StdGen)
  let (y,g3)       = randomR (-sy,sy) g2 :: (Float,StdGen)
  let (theta,g4)   = randomR (0,tau)  g3 :: (Float,StdGen)
  let (fliped,g5)  = random           g4 :: (Bool,StdGen)
  setStdGen g5
  return ((x,y),(fliped ,theta*tau))

  {-
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
                return $ Enemy (enemyOb,loc, ((10*(dx-0.5),10*(dy-0.5)),0.2*(dtheta-0.5))) 10 10 True --id means the pelet isn't movin
  -}
