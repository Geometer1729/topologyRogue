module Object where

import           Graphics.Gloss

type Object = [(Polygon,Color)]

type Polygon = [Point]

type Location = (Point,Orientation)

type Orientation = (Bool,Float)

testob::Object
testob =[([(10,10),(10,80),(80,80)],black),([(100,100),(100,180),(180,180)],red)]

objectToPicture :: Object -> Picture
objectToPicture o = Pictures $ map (\ (xs,c) -> color c $ Line xs) o

render::Object -> IO ()
render o = display (InWindow "Nice Window" (500,500) (0, 0)) white (objectToPicture o)
