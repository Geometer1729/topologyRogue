
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Debug.Trace
import Object
import SpaceRenderer

type TestWorld = [[Int]]

world :: TestWorld
world = [[1..s] | s <- [2..11]]

isDown :: KeyState -> Bool
isDown Down = True
isDown _ = False

handleEvent :: Event -> TestWorld -> TestWorld
handleEvent (EventKey k down mods f@(x,y)) t = case k of
                                              (Char m) -> if isDown down then trace (show m) t  else t
                                              (MouseButton LeftButton) -> trace (show x ++ "," ++ show y) t
                                              (SpecialKey _) -> t
handleEvent _ t = t

stepWorld :: Float -> TestWorld -> TestWorld
stepWorld _ t = t

testRender :: TestWorld -> Picture
testRender t = renderGrid 10 500 500

main :: IO ()
main = --play (InWindow "Nice Window" (500,500) (0, 0)) white 1 world testRender handleEvent stepWorld
       display (InWindow "Nice Window" (500,500) (0, 0)) white (renderWorld testWorld)
