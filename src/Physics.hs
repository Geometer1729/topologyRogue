module Physics where

import Object
import Collision
import Space
import Motion

imovable::Space -> MovingObj -> LocalObj -> MovingObj
imovable s moving still =
  where
    (_,tickedL,_) = tick s moving
    
