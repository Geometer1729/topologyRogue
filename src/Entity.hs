{-#LANGUAGE TypeSynonymInstances #-}
{-#LANGUAGE FlexibleInstances #-}

module Entity where


import Space
import Object
import Gameplay
import Graphics.Gloss

class Entity a where
  render :: Space -> a -> Picture
  movingObject :: a -> MovingObj
  entityTick :: World -> a -> a

instance Entity MovingObj where
  render s (o,l,m) = spaceDraw s (o,l)
  movingObject = id
  entityTick w = tick (space w)
