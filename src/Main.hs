
import           Debug.Trace
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Object
import           Space
import           Gameplay

windowWidth :: Float
windowWidth = 1200
windowHeight :: Float
windowHeight = 700

main :: IO ()
main = do
  world <- testPelletWorld windowWidth windowHeight
  playIO (InWindow "Grab Some Pellets, Friend" (round windowWidth,round windowHeight) (0,0)) white 30 world renderPelletWorld handlePelletWorld (gameplay windowWidth windowHeight)
        --play (InWindow "Nice Window" (500,500) (0, 0)) white 1 testWorld renderWorld handleEventWorld (stepWorld)
       --display (InWindow "Nice Window" (500,500) (0, 0)) white (renderWorld testWorld)
