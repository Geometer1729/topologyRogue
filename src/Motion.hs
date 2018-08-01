module Motion where

{-# LANGUAGE TypeSynonymInstances #-}

import Object
import Space
import Graphics.Gloss





maybeNeg::Bool->Float->Float
maybeNeg False = id
maybeNeg True = (*) (-1)


tick::Space -> MovingObj->MovingObj
tick s p@(o,l,m) = motReduce s (o, app m l, m)

testMovOb :: MovingObj
testMovOb = (testob,((0,0),(False,0::Float)),((0,0),0))

getLoc::MovingObj->LocalObj
getLoc (o,l,m) = (o,l)

app::Motion -> Location -> Location
app ((dx,dy),w) ((x,y),(f,t)) = ((x+dx,y+dy),(f,t+maybeNeg f w))

add:: Motion -> Motion -> Motion
add ((x1,y1),w1) ((x2,y2),w2) = ((x1+x2,y1+y2),w1+w2)

still::Motion
still = ((0,0),0)
