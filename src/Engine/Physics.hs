module Engine.Physics where

import Engine.Object
import Engine.Collision
import Engine.Space
import Engine.Motion

imovable::Space -> MovingObj -> LocalObj -> MovingObj
imovable s moving still =
  where
    (_,tickedL,_) = tick s moving
    
