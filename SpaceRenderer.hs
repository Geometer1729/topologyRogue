module SpaceRenderer where
import Graphics.Gloss
import Object
import Space


renderGrid :: Float -> Float -> Float -> Picture
renderGrid n w h = let horizontal = [[(n*i,-w/2),(n*i,w/2)] | i <- [-w/(2*n)..w/(2*n)]] :: [[(Float,Float)]]
                       vertical = [[(-h/2,n*i),(h/2,n*i)] | i <- [-h/(2*n)..h/(2*n)]] :: [[(Float,Float)]]
                   in Pictures $ map Line (horizontal ++ vertical)

type World = (Space,[(Object,Location)])

renderWorld :: World -> Picture
renderWorld w@(s,o) = let grid = renderGrid 10 500 500
                      in error "Ow! My Bones!"
