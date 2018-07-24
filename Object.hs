module Object where
{-# LANGUAGE FlexibleInstances    #-}


import           Data.Bits
import           Graphics.Gloss
type Object = [(Polygon,Color)]

type Polygon = [Point]

type Location = (Point,Orientation)

type Orientation = (Bool,Float)

testob::Object
testob =[([(-50,0),(0,50),(50,0)],black),([(-30,-5),(0,70),(30,-5)],red)]

objectToPicture :: Object -> Picture
objectToPicture o = Pictures $ map (\ (xs,c) -> color c $ Polygon xs) o

render::Object -> IO ()
render o = display (InWindow "Nice Window" (500,500) (0, 0)) white (objectToPicture o)

move:: Location -> Object -> Object
move (pt,(mi,theta)) o = obShift pt $ spin theta $ maybeMirror mi o

--movePt::Location->Point->Point
--movePt (pt,(mi,theta)) p = ptShift pt $ ptSpin theta $ (if mi then ptFlip else id) $ p 

mapPts::(Point -> Point) -> Object -> Object
mapPts f = map (\ (x,c) -> (map f x,c) )

maybeMirror::Bool -> Object -> Object
maybeMirror mi = if mi then mapPts ptFlip else id

ptFlip::Point -> Point
ptFlip (x,y) = (-x,y)

spin::Float -> Object->Object
spin theta = mapPts (ptSpin theta)

ptSpin::Float->Point->Point
ptSpin theta (x,y) = ( cos theta * x - sin theta * y,cos theta * y + sin theta * x)

obShift::Point -> Object -> Object
obShift p = mapPts (ptShift p)

ptShift::Point->Point->Point
ptShift (x1,y1) (x2,y2) = (x1+x2,y1+y2)

comp::Location->Location->Location
comp (v1,(m1,theta1)) (v2,(m2,theta2)) = (ptShift v1 v2,(  xor m1 m2 , theta1 + theta2 ))

getPts::Object->[Point]
getPts  o =  concat $ map fst o
