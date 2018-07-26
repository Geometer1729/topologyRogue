module Gameplay where
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Object
import           Space
import           System.Random


pelletTemplate :: Object
pelletTemplate = [(Circ ((0,0),10),red)]

data PelletWorld = PelletWorld {
                    pelletTaken :: Bool,
                    space       :: Space,
                    player      :: LocalObj,
                    pellet      :: LocalObj,
                    score       :: Int
                  }

testPelletWorld = do
              p <- getPellet 500
              return PelletWorld {pelletTaken=False,space=t2 250 250,player=testlocob,pellet=p,score=0}

getPellet :: Float -> IO LocalObj
getPellet size = do
              g <- getStdGen
              let (x,g2) = random g :: (Float,StdGen)
              let (y,g3) = random g2 :: (Float,StdGen)
              setStdGen g3
              let loc = ((size*(x-0.5),size*(y-0.5)),(False,0::Float))
              return (pelletTemplate,loc)


--stepWorld
gameplay :: Float -> PelletWorld -> IO PelletWorld
gameplay f w = do
              let s = space w
              let fixP = localReduce s (player w)
              p <- getPellet 500
              return PelletWorld {
                 pelletTaken=False,
                 space=s,
                 player = fixP,
                 pellet = if pelletTaken w then p else pellet w,
                 score = if pelletTaken w then score w + 1 else score w
               }

--render
renderPelletWorld :: PelletWorld -> IO Picture
renderPelletWorld w = do
                    let grid = renderGrid 10 500 500
                    let playerPic = spaceDraw (space w) (player w)
                    let pelletPic = spaceDraw (space w) (pellet w)
                    return $ Pictures [grid,playerPic,pelletPic,renderScore w]

renderScore :: PelletWorld -> Picture
renderScore p = let s = score p
                    pic = Text ("Score: " ++ (show s))
                    scaled = Scale 0.2 0.2 pic
                    translate = Translate (-75) 75 scaled
                in translate

--input
handlePelletWorld :: Event -> PelletWorld -> IO PelletWorld
handlePelletWorld k w = do
                        let playerObj = fst (player w)
                        let playerLoc = snd (player w)
                        let newLoc = keyPressMove k playerLoc
                        let c = traceThis $ collides (space w) (player w) (pellet w)
                        return PelletWorld {
                          pelletTaken = c, --use collision detection here
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
