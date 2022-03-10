module Grid where

import Data.Array
import Graphics.Gloss

backgroundColor = makeColorI 94 80 134 255

type Point = (Int, Int)

newtype Grid = Grid (Array (Int, Int) Cell)
    deriving (Eq, Show)

data Cell = Empty | Orange | White | Falling
    deriving (Eq, Show)

cellSize = 20

gridWidth = 16
gridHeight = 12   -- 10 proper rows but we allocate the top two rows for dropping space

emptyGrid :: Grid
emptyGrid = Grid (array ((1,1),(gridWidth,gridHeight)) [((x,y),Empty) | x<-[1..gridWidth], y<-[1..gridHeight]])

drawGrid :: Grid -> Picture
drawGrid grid = pictures [drawCell x y | x <- [1..gridWidth], y <- [1..gridHeight]]
    where
        drawCell x y = pictures [ translate (fromIntegral (x-1) * cellSize)
                         (fromIntegral (y-1) * cellSize)
                         (color orange (rectangleWire cellSize cellSize))]
