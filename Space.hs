module Space where
import           Debug.Trace
import           Graphics.Gloss
import           Object
import           Prelude        hiding (lines)

traceThis:: (Show a) => a -> a
traceThis x = trace (show x) x

testlocob :: LocalObj
testlocob = (testob,((0,0),(False,0::Float)))

type Space = [(Boundary,Rule)]

type Boundary = Location -> Bool

type Rule = Location -> Location

still::Location
still = ((0,0),(False,0))

vecToLoc::Point->Location
vecToLoc x = (x,(False,0))

wrapX::Float -> Float -> Space
wrapX l h = [ ( (\ ((x,y),_) -> x < l) , comp (vecToLoc (h-l,0))) , ( (\ ((x,y),_) -> x > h) , comp (vecToLoc (l-h,0))) ]

wrapY::Float -> Float -> Space
wrapY l h = [ ( (\ ((x,y),_) -> y < l) , comp (vecToLoc (0,h-l))) , ( (\ ((x,y),_) -> y > h) ,comp (vecToLoc (0,l-h))) ]

flipX::Float->Float -> Space
flipX l h = [ ( (\ ((x,y),_) -> x < l) , comp ((h-l,0),(True,pi)) ),( (\ ((x,y),_) -> x > h) , comp ((l-h,0),(True,pi)) ) ]

flipY::Float->Float -> Space
flipY l h = [ ( (\ ((x,y),_) -> y < l) , comp ((0,h-l),(True,0)) ),( (\ ((x,y),_) -> y > h) , comp ((0,l-h),(True,0)) ) ]


t2::Float ->Float ->Space
t2 w h = wrapX (-w) w ++ wrapY (-h) h

kh::Float ->Float ->Space
kh w h = flipX (-w) w ++ wrapY (-h) h

kv::Float ->Float ->Space
kv w h = wrapX (-w) w ++ flipY (-h) h

rp2::Float ->Float ->Space
rp2 w h = flipX (-w) w ++ flipY (-h) h

spaceAdd::Space->Location->Location->Location
spaceAdd s v1 v2 = spaceReduce s $ comp v1 v2

spaceReduce::Space->Location->Location
spaceReduce s v = if spaceCheck s v then spaceReduce s $ appSpace s v else v

appSpace::Space->Location->Location
appSpace [] p         = p
appSpace ((b,r):xs) p = if b p then appSpace xs $ r p else appSpace xs p

spaceCheck::Space->Location->Bool
spaceCheck s p = or [ fst r p | r <- s]

dups::Space->LocalObj->[LocalObj] -- takes a space and an object produces a list of posible render positons of that object
dups s o = rollingDups s [o]

rollingDups::Space->[LocalObj]->[LocalObj]
rollingDups [] os = os
rollingDups ((b,r):ns) os = if colides then rollingDups ns $ os ++ map (\ (o,l) -> (o, r l)) os else rollingDups ns os
  where
    colides =  or . (map (b . vecToLoc)) . getPts . concat $ map (uncurry .flip $ move) os :: Bool

spaceDraw::Space-> LocalObj -> Picture
spaceDraw s o = Pictures . (map objectToPicture) . map (uncurry (flip move)) $ dups s o

localReduce::Space->LocalObj->LocalObj
localReduce s = fmap (spaceReduce s)

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
shapeColision (Circ (pt1,r1)) (Circ (pt2,r2)) = l2 pt1 pt2 < r1 + r2
shapeColision p1@(Pol pts1) p2@(Pol pts2) = (or [contains p1 pt | pt <- pts2 ]) || (or [contains p2 pt | pt <- pts1 ])
shapeColision s@(Circ (c,r)) p@(Pol pts) = or $ [contains p c]++[traceShowId $ circleHasLine s l | l <- lines p]
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
circleHasLine (Circ (c,r)) (p1,p2) = ((min d1 d2) < r) || and [ (h-3 < r),b>0,b<d1]
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
