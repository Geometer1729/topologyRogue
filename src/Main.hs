
import Debug.Trace
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Engine.Object
import Engine.Space
import Game.Gameplay
import Engine.Definitions



main :: IO ()
main = do
  world <- testPelletWorld (kh (windowWidth/2) (windowHeight/2)) windowWidth windowHeight
  playIO  (InWindow "Game" (1600, 900) (50, 50)) black 30 world renderPelletWorld handlePelletWorld (gameplay windowWidth windowHeight)
        --(InWindow "Grab Some Pellets, Friend" (round windowWidth,round windowHeight)
        --play (InWindow "Nice Window" (500,500) (0, 0)) white 1 testWorld renderWorld handleEventWorld (stepWorld)
       --display (InWindow "Nice Window" (500,500) (0, 0)) white (renderWorld testWorld)
