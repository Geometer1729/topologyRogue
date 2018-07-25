module Gameplay where
import Graphics.Gloss
import System.Random
import Object
import Space

pelletTemplate = [([(0,0),(0,10),(10,0),(10,10)] :: [Point],red),
                  ([(5,0),(10,5),(5,10),(0,5)] :: [Point],black)]

data PelletWorld = PelletWorld {
                    pelletTaken :: Bool,
                    space :: Space,
                    player :: LocalObj,
                    pellet :: IO LocalObj
                  }

testPelletWorld = PelletWorld {pelletTaken=False,space=t2 500 500,player=testlocob,pellet=getPellet 0 500}

getPellet :: Float -> Int -> IO LocalObj
getPellet f size = do
              g <- getStdGen
              let (x,g2) = random g :: (Float,StdGen)
              let (y,g3) = random g2 :: (Float,StdGen)
              let loc = ((x,y),(False,0::Float))
              return (pelletTemplate,loc)



gameplay :: Float -> IO PelletWorld -> IO PelletWorld
gameplay f = error "go fuck yourself"
