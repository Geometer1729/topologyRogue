module Space where
import           Graphics.Gloss
import           Object

type Space = [(Boundry,Rule)]

type Boundry = Point -> Bool

type Rule = Point -> Point

wrapX::Float -> Float -> Space
wrapX l h = [ ( (\ (x,y) -> x < l) , ptShift (h-l,0)) , ( (\ (x,y) -> x > h) , ptShift (l-h,0)) ]

wrapY::Float -> Float -> Space
wrapY l h = [ ( (\ (x,y) -> y < l) , ptShift (0,h-l)) , ( (\ (x,y) -> y > h) , ptShift (0,l-h)) ]

t2::Float ->Float ->Space
t2 w h = wrapX (-w) w ++ wrapY (-h) h

spaceAdd::Space->Point->Point->Point
spaceAdd s v1 v2 = spaceReduce s $ ptShift v1 v2

spaceReduce::Space->Point->Point
spaceReduce s v = if spaceCheck s v then spaceReduce s $ appSpace s v else v

appSpace::Space->Point->Point
appSpace [] p         = p
appSpace ((b,r):xs) p = if b p then appSpace xs $ r p else appSpace xs p

spaceCheck::Space->Point->Bool
spaceCheck s p = or [ fst r p | r <- s]

dups::Space->Object->[Object] -- takes a space and an object produces a list of posible render positons of that object
dups s o = rollingDups s [o]

rollingDups::Space->[Object]->[Object]
rollingDups [] o = o
rollingDups ((b,r):ns) o = if colides then rollingDups ns $ concat $ map (\x -> [x,mapPts r x ]) o else rollingDups ns o
  where
    colides = or . (map b) . getPts . concat $ o :: Bool

spaceDraw::Space->Object->Picture
spaceDraw s o = Pictures . (map objectToPicture) $ dups s o
