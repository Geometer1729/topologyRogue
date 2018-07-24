module Space where
import           Graphics.Gloss
import           Object

type Space = [(Boundry,Rule)]

type Boundry = Location -> Bool

type Rule = Location -> Location

still::Location
still = ((0,0),(False,0))

vecToLoc::Point->Location
vecToLoc x = (x,(False,0))

wrapX::Float -> Float -> Space
wrapX l h = [ ( (\ ((x,y),_) -> x < l) , comp (vecToLoc (h-l,0))) , ( (\ ((x,y),_) -> x > h) , comp (vecToLoc (l-h,0))) ]

wrapY::Float -> Float -> Space
wrapY l h = [ ( (\ ((x,y),_) -> y < l) , comp (vecToLoc (0,h-l))) , ( (\ ((x,y),_) -> y > h) ,comp (vecToLoc (0,l-h))) ]

t2::Float ->Float ->Space
t2 w h = wrapX (-w) w ++ wrapY (-h) h

spaceAdd::Space->Location->Location->Location
spaceAdd s v1 v2 = spaceReduce s $ comp v1 v2

spaceReduce::Space->Location->Location
spaceReduce s v = if spaceCheck s v then spaceReduce s $ appSpace s v else v

appSpace::Space->Location->Location
appSpace [] p         = p
appSpace ((b,r):xs) p = if b p then appSpace xs $ r p else appSpace xs p

spaceCheck::Space->Location->Bool
spaceCheck s p = or [ fst r p | r <- s]

dups::Space->(Object,Location)->[(Object,Location)] -- takes a space and an object produces a list of posible render positons of that object
dups s o = rollingDups s [o]

rollingDups::Space->[(Object,Location)]->[(Object,Location)]
rollingDups [] os = os
rollingDups ((b,r):ns) os = if colides then rollingDups ns $ map (\ (o,l) -> (o, r l)) os else rollingDups ns os
  where
    colides = or . (map b) . (map vecToLoc) . getPts . concat $ map fst os :: Bool

spaceDraw::Space->Object-> Location -> Picture
spaceDraw s o l = Pictures . (map objectToPicture) . map (uncurry (flip move)) $ dups s (o,l)
