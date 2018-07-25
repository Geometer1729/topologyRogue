module Gameplay where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random
import Object
import Space


pelletTemplate :: Object
pelletTemplate = [(Pol [(0,0),(10,0),(10,10),(0,10)],red),
                  (Pol [(5,0),(10,5),(5,10),(0,5)],black)]

data PelletWorld = PelletWorld {
                    pelletTaken :: Bool,
                    space :: Space,
                    player :: LocalObj,
                    pellet :: IO LocalObj,
                    score :: Int
                  }

testPelletWorld = PelletWorld {pelletTaken=False,space=t2 250 250,player=testlocob,pellet=getPellet 0 500,score=0}

getPellet :: Float -> Int -> IO LocalObj
getPellet f size = do
              g <- getStdGen
              let (x,g2) = random g :: (Float,StdGen)
              let (y,g3) = random g2 :: (Float,StdGen)
              let loc = ((x,y),(False,0::Float))
              return (pelletTemplate,loc)


--stepWorld
gameplay :: Float -> PelletWorld -> IO PelletWorld
gameplay f w = do
              let s = space w
              let fixP = localReduce s (player w)
              return PelletWorld {
                 pelletTaken=False,
                 space=s,
                 player = fixP,
                 pellet = if pelletTaken w then getPellet 0 500 else pellet w,
                 score = if pelletTaken w then score w + 1 else score w
               }

--render
renderPelletWorld :: PelletWorld -> IO Picture
renderPelletWorld w = do
                    let grid = renderGrid 10 500 500
                    let playerPic = spaceDraw (space w) (player w)
                    p <- pellet w
                    let pelletPic = spaceDraw (space w) p
                    return $ Pictures [playerPic,pelletPic,grid]

--input
handlePelletWorld :: Event -> PelletWorld -> IO PelletWorld
handlePelletWorld k w = let playerObj = fst (player w)
                            playerLoc = snd (player w)
                            newLoc = keyPressMove k playerLoc
                        in return PelletWorld {
                          pelletTaken=False, --use collision detection here
                          space = space w,
                          player = (playerObj,newLoc),
                          pellet = pellet w,
                          score = score w
                        }



keyPressMove :: Event -> Location -> Location
keyPressMove (EventKey k downkey mods f@(x,y)) l = case k of
                                                    (SpecialKey KeyUp) -> comp (up 10) l
                                                    (SpecialKey KeyDown) -> comp (down 10) l
                                                    (SpecialKey KeyLeft) -> comp (left 10) l
                                                    (SpecialKey KeyRight) -> comp (right 10) l
                                                    _ -> l
keyPressMove _ l = l
