
import           Debug.Trace
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Object
import           Space
import           SpaceRenderer


main :: IO ()
main = play (InWindow "Nice Window" (500,500) (0, 0)) white 1 testWorld renderWorld handleEventWorld (stepWorld)
       --display (InWindow "Nice Window" (500,500) (0, 0)) white (renderWorld testWorld)
