module Object where
{-# LANGUAGE FlexibleInstances    #-}


import           Data.Bits
import           Graphics.Gloss

type LocalObj = (Object,Location)

data Shape = Pol Polygon | Circ Circle

type Circle = (Point,Float)

type Part = (Shape,Color)

type Object = [Part]

type Polygon = [Point]

type Location = (Point,Orientation)

type Orientation = (Bool,Float)

testob::Object
testob =[(Pol [(-50,0),(0,50),(50,0)],black) , (Pol [(-30,-5),(0,70),(30,-5)],red) , (Circ ((0,50),25) , blue ) , (Circ ((50,0),10) , cyan )]

objectToPicture :: Object -> Picture
objectToPicture o = Pictures $ map (\ (xs,c) -> color c $ drawShape xs) o

drawShape:: Shape -> Picture
drawShape (Pol p) = Polygon p
drawShape (Circ ((x,y),r)) = translate x y (circleSolid r)

render::Object -> IO ()
render o = display (InWindow "Nice Window" (500,500) (0, 0)) white (objectToPicture o)

move:: Location -> Object -> Object
move (pt,(mi,theta)) o = obShift pt $ spin theta $ maybeMirror mi o

place::LocalObj->Object
place = uncurry (flip move)

--movePt::Location->Point->Point
--movePt (pt,(mi,theta)) p = ptShift pt $ ptSpin theta $ (if mi then ptFlip else id) $ p

mapPts::(Point -> Point) -> Object -> Object
mapPts _ [] = []
mapPts f (((Pol pts),c):o)  = (Pol (map f pts),c) : mapPts f o
mapPts f ((Circ(pt,r),c):o) = (Circ (f pt,r),c) : mapPts f o

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
getPts  o = concat $ map partFlags o

partFlags::Part->[Point]
partFlags (Pol pts,_) = pts
partFlags (Circ (pt,r),_) = let fs = (map ptShift (cardinals r))
                        in [f pt | f <- fs]


cardinals::Float -> [Point] -- In the event of spaces with non Vertical/Horizontal boundries this will need to include more pts
cardinals x = [(x,0),(-x,0),(0,x),(0,-x)]

renderGrid :: Float -> Float -> Float -> Picture
renderGrid n w h = let horizontal = [[(n*i,-w/2),(n*i,w/2)] | i <- [-w/(2*n)..w/(2*n)]] :: [[(Float,Float)]]
                       vertical = [[(-h/2,n*i),(h/2,n*i)] | i <- [-h/(2*n)..h/(2*n)]] :: [[(Float,Float)]]
                   in Pictures $ map Line (horizontal ++ vertical)

--Locations for moving
up :: Float -> Location
up x = ((0,x),(False,0 :: Float))
down :: Float -> Location
down x = ((0,-x),(False,0 :: Float))
left :: Float -> Location
left x = ((-x,0),(False,0 :: Float))
right :: Float -> Location
right x = ((x,0),(False,0 :: Float))
