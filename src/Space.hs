module Space where
import           Graphics.Gloss
import           Object

testlocob :: LocalObj
testlocob = (testob,((0,0),(False,0::Float)))

localBullet:: LocalObj
localBullet = (bulletOb,((0,0),(False,0::Float)))

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
flipX l h = [ ( (\ ((x,y),_) -> x < l) , (\ ((x,y),(f,t)) -> (((x+h-l,h+l-y),(not f,pi-t)  )  ) ) )
            , ( (\ ((x,y),_) -> x > h) , (\ ((x,y),(f,t)) -> (((x+l-h,h+l-y),(not f,pi-t)  )  ) ) )]

flipY::Float->Float -> Space
flipY l h = [ ( (\ ((x,y),_) -> y < l) , (\ ((x,y),(f,t)) -> (((h+l-x,y+h-l),(not f,t)  )  ) ) )
            , ( (\ ((x,y),_) -> y > h) , (\ ((x,y),(f,t)) -> (((h+l-x,y+l-h),(not f,t)  )  ) ) )]

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

rollingDups :: Space -> [LocalObj] -> [LocalObj]
rollingDups [] objects = objects
rollingDups ((boundary, rule):next) objects = if collides then rollingDups next $ objects ++ map (\ (object, location) -> (object, rule location)) objects else rollingDups next objects
  where
    collides =  or . (map (boundary . vecToLoc)) . getPts . concat $ moveLocalObjects objects :: Bool

spaceDraw :: Space -> LocalObj -> Picture
spaceDraw s o = Pictures . (map objectToPicture) . moveLocalObjects $ dups s o

moveLocalObjects = map (uncurry . flip $ move) 

localReduce::Space->LocalObj->LocalObj
localReduce s = fmap (spaceReduce s)
