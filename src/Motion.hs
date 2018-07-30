import Object
import Space

type Motion = Location -> Location

type MovingObj = (Motion,Location,Object)

tick::Space -> MovingObj->MovingObj
tick s (m,l,o) = (m,spaceReduce s (m l), o)
