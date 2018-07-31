module Motion where

import Object
import Space

type Motion = Location -> Location

type MovingObj = (Object,Location,Motion)

tick::Space -> MovingObj->MovingObj
tick s (o,l,m) = (o, spaceReduce s (m l) , m)

testMovOb :: MovingObj
testMovOb = (testob,((0,0),(False,0::Float)),id)

getLoc::MovingObj->LocalObj
getLoc (o,l,m) = (o,l)

mup :: Float -> Motion
mup x = comp (up x)
mdown :: Float -> Motion
mdown x = comp (down x)
mright :: Float -> Motion
mright x = comp (right x)
mleft :: Float -> Motion
mleft x = comp (left x)
