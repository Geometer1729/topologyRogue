module Space where
import           Graphics.Gloss
import           Object

type Space = [(Boundry,Rule)]

type Boundry = Point -> Bool

type Rule = Point -> Point

wrapX::Float -> Space
wrapX w = [ ( (\ (x,y) -> x < 0) , ptShift (w,0)) , ( (\ (x,y) -> x > w) , ptShift (-w,0)) ]

wrapY::Float -> Space
wrapY w =  [ ( (\ (x,y) -> y < 0) , ptShift (0,w)) , ( (\ (x,y) -> y > w) , ptShift (0,-w)) ]

t2::Float ->Float ->Space
t2 w h = wrapX w ++ wrapY h

spaceAdd::Space->Point->Point->Point
spaceAdd s v1 v2 = spaceReduce s $ ptShift v1 v2

spaceReduce::Space->Point->Point
spaceReduce s v = if spaceCheck s v then spaceReduce s $ appSpace s v else v

appSpace::Space->Point->Point
appSpace [] p         = p
appSpace ((b,r):xs) p = if b p then appSpace xs $ r p else appSpace xs p

spaceCheck::Space->Point->Bool
spaceCheck s p = or [ fst r p | r <- s]
