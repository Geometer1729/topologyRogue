
import           Debug.Trace
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Object
import           Space
import           Gameplay


main :: IO ()
main = playIO (InWindow "Grab Some Pellets, Friend" (500,500) (0,0)) white 1 testPelletWorld renderPelletWorld handlePelletWorld gameplay
        --play (InWindow "Nice Window" (500,500) (0, 0)) white 1 testWorld renderWorld handleEventWorld (stepWorld)
       --display (InWindow "Nice Window" (500,500) (0, 0)) white (renderWorld testWorld)
