module Engine.Space where
import Engine.Definitions
import Graphics.Gloss
import Engine.Object





vecToLoc::Point->Location
vecToLoc x = (x,(False,0))

wrapX::Float -> Float -> Space
wrapX l h = [ ( (\ ((x,y),_) -> x < l) , comp (vecToLoc (h-l,0)) , id)
            , ( (\ ((x,y),_) -> x > h) , comp (vecToLoc (l-h,0)) , id) ]

wrapY::Float -> Float -> Space
wrapY l h = [ ( (\ ((x,y),_) -> y < l) , comp (vecToLoc (0,h-l)), id)
            , ( (\ ((x,y),_) -> y > h) , comp (vecToLoc (0,l-h)), id ) ]

flipX::Float->Float -> Space
flipX l h = [leftRule,rightRule]
  where
    leftRule  = (leftCon,leftLocRule,leftMotRule)
    rightRule = (rightCon,rightLocRule,rightMotRule)
    leftCon ((x,y),_) = x < l
    rightCon ((x,y),_) = x > h
    leftLocRule  ((x,y),(f,t)) = ((x+h-l,h+l-y),(not f,-t))
    rightLocRule ((x,y),(f,t)) = ((x+l-h,h+l-y),(not f,-t))
    leftMotRule  ((x,y),r) = ((x,-y),r)
    rightMotRule ((x,y),r) = ((x,-y),r)

flipY::Float -> Float -> Space
flipY l h = [topRule,bottomRule]
  where
    topRule  = (topCon,topLocRule,topMotRule)
    bottomRule = (bottomCon,bottomLocRule,bottomMotRule)
    topCon        ((x,y),_)     = y > h
    bottomCon     ((x,y),_)     = y < l
    topLocRule    ((x,y),(f,t)) = ((h+l-x,y-h+l),(not f,tau/2-t))
    bottomLocRule ((x,y),(f,t)) = ((h+l-x,y+h-l),(not f,tau/2-t))
    topMotRule    ((x,y),r)     = ((-x,y),r)
    bottomMotRule ((x,y),r)     = ((-x,y),r)

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


motReduce::Space->MovingObj->MovingObj
motReduce s p@(o,l,m) = if spaceCheck s l then motReduce s $ appMot s p else p

appSpace::Space->Location->Location
appSpace [] p         = p
appSpace ((b,r,_):xs) p = if b p then appSpace xs $ r p else appSpace xs p

appMot::Space->MovingObj->MovingObj
appMot [] p = p
appMot ((b,lr,mr):xs) (o,l,m) = if b l then appMot xs (o,lr l,mr m) else appMot xs (o,l,m)

spaceCheck::Space->Location->Bool
spaceCheck s p = or [ r p | (r,_,_) <- s]

dups::Space->LocalObj->[LocalObj] -- takes a space and an object produces a list of posible render positons of that object
dups s o = rollingDups s [o]

rollingDups :: Space -> [LocalObj] -> [LocalObj]
rollingDups [] objects = objects
rollingDups ((boundary, rule,_):next) objects = if collides then rollingDups next $ objects ++ map (\ (object, location) -> (object, rule location)) objects else rollingDups next objects
  where
    collides =  or . (map (boundary . vecToLoc)) . getPts . concat $ moveLocalObjects objects :: Bool

spaceDraw :: Space -> LocalObj -> Picture
spaceDraw s o = Pictures . (map objectToPicture) . moveLocalObjects $ dups s o

moveLocalObjects = map (uncurry . flip $ move)

localReduce::Space->LocalObj->LocalObj
localReduce s = fmap (spaceReduce s)

--Former module Motion.hs

maybeNeg::Bool->Float->Float
maybeNeg False = id
maybeNeg True = (*) (-1)

tick::Space -> MovingObj->MovingObj
tick s p@(o,l,m) = motReduce s (o, app m l, m)

test0= [(Pol [(83,247),(86,340),(281,405),(238,305),(230,219)],makeColorI 63 72 204 255),(Pol [(29,196),(173,228),(230,218),(234,240),(346,262),(448,129),(240,174),(247,148),(179,9)],makeColorI 237 28 36 255)]


testMovOb :: MovingObj
testMovOb = (testob ,((0,0),(False,0::Float)),((0,0),0))

getLoc::MovingObj->LocalObj
getLoc (o,l,m) = (o,l)

app::Motion -> Location -> Location
app ((dx,dy),w) ((x,y),(f,t)) = ((x+dx,y+dy),(f,t+maybeNeg f w))

add:: Motion -> Motion -> Motion
add ((x1,y1),w1) ((x2,y2),w2) = ((x1+x2,y1+y2),w1+w2)

setMot::Motion -> MovingObj -> MovingObj
setMot m (o,l,_) = (o,l,m)

ccw::Float -> Motion
ccw dtheta = ((0,0),dtheta)

cw::Float ->  Motion
cw x = ccw (-1*x)

forward::Float -> Location -> Motion
forward x (_,(f,theta)) = ((cos theta * x,sin theta * x),0)

backward::Float -> Location -> Motion
backward x = forward (-1*x)

right::Float -> Location -> Motion
right x (_,(f,theta)) = ((cos thetaR * x,sin thetaR * x),0)
  where
    thetaR = theta - tau/4

left:: Float -> Location -> Motion
left x = right (-1*x)


spaceNorm::Space->Location->Location->Float
spaceNorm s loc1 loc2 = minimum [l2 p1 p2 |  (p1,_) <- strongDups s loc1 , (p2,_) <- strongDups s loc2 ]


strongDups::Space->Location->[Location]
strongDups [] l = [l]
strongDups ((_,lr,_):rs) l = filterDups $ concat $ map (strongDups rs) [l,lr l]

filterDups::(Eq a) => [a] -> [a]
filterDups [] = []
filterDups (x:xs) = if elem x xs then filterDups xs else x : filterDups xs
