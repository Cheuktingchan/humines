module Grid where

import Data.Array
import Graphics.Gloss
import Settings
import Data.Function ((&))

newtype Grid = Grid (Array (Int, Int) Cell)
    deriving (Eq, Show)

data CellObject = Empty | Orange | White
    deriving (Eq, Show)

data CellState = None | Falling | Solid
    deriving (Eq, Show)

data Cell = Cell {
    cellObject :: CellObject,
    cellState :: CellState,
    controlling :: Bool
}

    deriving (Eq, Show)

cellSize = 40

getCell :: Grid -> Coord -> Cell
getCell (Grid gridArr) coords = gridArr ! coords

setCell :: Cell -> Coord -> Grid -> Grid
setCell cell coord (Grid gridArr) = Grid $ gridArr // [(coord,cell)]

gridWidth = 16
gridHeight = 12   -- 10 proper rows but we allocate the top two rows for dropping space

emptyGrid :: Grid
emptyGrid = Grid (array ((1,1),(gridWidth,gridHeight)) [((x,y), Cell {cellObject = Empty, cellState = None, controlling = False}) | x<-[1..gridWidth], y<-[1..gridHeight]])

makeGrid :: Grid -> [Coord] -> Grid
makeGrid grid fallingCoords = foldr (setCell Cell {cellObject = Orange, cellState = Falling, controlling = True}) grid fallingCoords

drawGrid :: Grid -> Picture
drawGrid grid = pictures [drawCell x y | x <- [1..gridWidth], y <- [1..gridHeight]]
    where
        drawCell x y = translate (fromIntegral x * cellSize - fromIntegral (windowWidth `div` 2 - gridWidth * 10)) (fromIntegral y * 40 - fromIntegral (windowHeight `div` 2 - gridHeight * 6)) $

{-             case getCellObject of
                Empty   -> pictures [
                            color haskellColor $ rectangleSolid cellSize cellSize,
                            color black $ rectangleWire cellSize cellSize]
                Orange  -> color orange $ rectangleSolid cellSize cellSize

                White   -> color orange $ rectangleSolid cellSize cellSize -}
            case getCellState of
                None   -> pictures [
                            color haskellColor $ rectangleSolid cellSize cellSize,
                            color black $ rectangleWire cellSize cellSize]
                Falling  -> color orange $ rectangleSolid cellSize cellSize

                Solid   -> color white $ rectangleSolid cellSize cellSize
                where
                    getCellObject = cellObject $ getCell grid (x,y)
                    getCellState = cellState $ getCell grid (x,y)
