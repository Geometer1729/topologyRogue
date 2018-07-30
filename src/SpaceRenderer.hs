module SpaceRenderer where
import           Debug.Trace
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Game
import           Object
import           Space

isDown :: KeyState -> Bool
isDown Down = True
isDown _    = False


{-
renderGrid :: Float -> Float -> Float -> Picture
renderGrid n w h = let horizontal = [[(n*i,-w/2),(n*i,w/2)] | i <- [-w/(2*n)..w/(2*n)]] :: [[(Float,Float)]]
                       vertical = [[(-h/2,n*i),(h/2,n*i)] | i <- [-h/(2*n)..h/(2*n)]] :: [[(Float,Float)]]
                   in Pictures $ map Line (horizontal ++ vertical)
-}
type World = (Space,[LocalObj])

renderWorld :: World -> Picture
renderWorld w@(s,o) = let grid = renderGrid 10 500 500
                          pieces = [spaceDraw s p | p <- o]
                      in Pictures ([grid] ++ pieces)

testWorld = ((t2 250 250),[(testob,((0::Float,0::Float),(False,0::Float)))])

handleEventWorld :: Event -> World -> World
handleEventWorld (EventKey k downkey mods f@(x,y)) t@(s,os) = case k of
                                              (Char m) -> if isDown downkey then trace (show m) t  else t
                                              (MouseButton LeftButton) -> trace (show x ++ "," ++ show y) t
                                              (SpecialKey KeyUp) -> (s,[(fst o,comp (up 10) (snd o)) | o <- os])
                                              (SpecialKey KeyDown) -> (s,[(fst o,comp (down 10) (snd o)) | o <- os])
                                              (SpecialKey KeyLeft) -> (s,[(fst o,comp (left 10) (snd o)) | o <- os])
                                              (SpecialKey KeyRight) -> (s,[(fst o,comp (right 10) (snd o)) | o <- os])
                                              _ -> t
handleEventWorld _ t = t

stepWorld :: Float -> World -> World
stepWorld _ (s,os) =  (s , (map (localReduce s)) os)
