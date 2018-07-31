-- Seems to need a little debuging
module Collision where

import Graphics.Gloss
import Space
import Object
import           Prelude        hiding (lines)

collides::Space->LocalObj->LocalObj->Bool --assumes space reduced objects
collides s r1 r2 = or [simpleCollides (place x) (place y) | x <- (dups s r1) , y <- (dups s r2) ]

simpleCollides::Object->Object->Bool
simpleCollides o1 o2 = or [partColision p1 p2 | p1 <- o1 , p2 <- o2 ]

partColision::Part->Part->Bool -- You couldmake color dependent collision if you want
partColision (s1,_) (s2,_) = shapeColision s1 s2

isCirc :: Shape -> Bool
isCirc (Circ _) = True
isCirc (Pol _)  = False
isCirc _        = error "What the fuck?"

shapeColision::Shape->Shape->Bool
shapeColision (Circ (pt1,r1)) (Circ (pt2,r2)) =  (l2 pt1 pt2) < (r1 + r2) -3
shapeColision p1@(Pol pts1) p2@(Pol pts2) = (or [contains p1 pt | pt <- pts2 ]) || (or [contains p2 pt | pt <- pts1 ])
shapeColision s@(Circ (c,r)) p@(Pol pts) = or $ [contains p c]++[circleHasLine s l | l <- lines p]
shapeColision a b = shapeColision b a

l2::Point->Point->Float
l2 (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2

lines::Shape->[(Point,Point)]
lines (Pol (x1:xs)) = lineHelper (x1:xs) x1

lineHelper::[Point]->Point->[(Point,Point)]
lineHelper (x1:x2:xs) f = (x1,x2): lineHelper (x2:xs) f
lineHelper (x1:[]) f    = [(x1,f)]

contains::Shape->Point->Bool
contains (Circ (c,r)) p = l2 c p < r
contains q@(Pol _) p = odd $ length $ filter id [triangleHas tri p | tri <- (triangulate q)]

triangulate::Shape->[Shape]
triangulate (Circ _) = error "You can only triangulate polygons"
triangulate (Pol (x1:x2:x3:xs)) = (Pol [x1,x2,x3]) : triangulate (Pol (x1:x3:xs))
triangulate (Pol (x1:x2:[])) = []

triangleHas::Shape->Point->Bool
triangleHas (Pol ((x1,y1):(x2,y2):(x3,y3):[])) (x,y) = and [s>0,t>0,t+s<1]
  where
    s = 1/(2*a)*(y1*x3 - x1*y3 + (y3 - y1)*x + (x1 - x3)*y)
    t = 1/(2*a)*(x1*y2 - y1*x2 + (y1 - y2)*x + (x2 - x1)*y)
    a = 0.5 *(-y2*x3 + y1*(-x2 + x3) + x1*(y2 - y3) + x2*y3)
triangleHas _ _ = error "triangleHas is only for triangles"

circleHasLine::Shape->(Point,Point)->Bool
circleHasLine (Circ (c,r)) (p1,p2) = ((min d2 (l2 c p2)) < r) || and [ (h-3 < r),b>0,b<d1]
  where
    v1 = ptSub p2 p1 :: Point
    d1 = (l2 (0,0) v1) :: Float
    v2 = ptSub c p1 ::Point
    d2 = (l2 (0,0) v2) ::Float
    d = ptDot v1 v2 ::Float
    b = d/ d1 :: Float
    h = sqrt $ d2^2 - b^2 ::Float
circleHasLine _ _ = error "circleHasLine expects a circle"

ptSub::Point->Point->Point
ptSub (x1,y1) (x2,y2) = (x1-x2,y1-y2)

ptDot::Point -> Point -> Float
ptDot (x1,y1) (x2,y2) = x1*x2+y1*y2
