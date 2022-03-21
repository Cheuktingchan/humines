module Grid where

import Data.Array
import Graphics.Gloss
import Settings
import Data.Maybe ( fromMaybe )

-- grid and cell data type definitions
newtype Grid = Grid (Array (Int, Int) Cell)
    deriving (Eq, Show)

data CellObject = Empty | Orange | White
    deriving (Eq, Show)

data CellState = None | Falling | Solid
    deriving (Eq, Show)

data Cell = Cell {
    cellObject :: CellObject,
    cellState :: CellState,
    toDestroy :: Bool
}

    deriving (Eq, Show)

cellSize = 40

-- safe getter for cells
getCell :: Grid -> Coord -> Maybe Cell
getCell (Grid gridArr) (x,y)
    | x > gridWidth || x < 1 || y > gridHeight || y < 1 = Nothing
    | otherwise                                         = Just (gridArr ! (x,y))

-- setter for cells
setCell :: Cell -> Coord -> Grid -> Grid
setCell cell coord (Grid gridArr) = Grid $ gridArr // [(coord,cell)]

gridWidth = 16
gridHeight = 12   -- 10 proper rows but we allocate the top two rows for dropping blocks

-- initial empty grid
emptyCell :: Cell
emptyCell = Cell {cellObject = Empty, cellState = None, toDestroy = False}
emptyGrid :: Grid
emptyGrid = Grid (array ((1,1),(gridWidth,gridHeight)) [((x,y), emptyCell) | x<-[1..gridWidth], y<-[1..gridHeight]])

-- make the grid for drawing including the controlling block on the top row (which is not on the world grid)
makeGrid :: [CellObject] -> [Coord] -> Grid -> Grid
makeGrid [] _ grid = grid
makeGrid _ [] grid = grid
makeGrid (object:objects) (coord:coords) grid = makeGrid objects coords (setCell Cell {cellObject = object, cellState = Falling, toDestroy = False} coord grid)

drawGrid :: Color -> Grid -> Picture
drawGrid bgColor grid = pictures [drawCell x y | x <- [1..gridWidth], y <- [1..gridHeight]]
    where
        -- draw cells depending on each cell object
        drawCell x y = translate (fromIntegral x * cellSize - fromIntegral (windowWidth `div` 2 - gridWidth * 10)) (fromIntegral y * 40 - fromIntegral (windowHeight `div` 2 - gridHeight * 6)) $

            case getCellObject of
                Empty   -> pictures [ -- draw empty cell
                            color bgColorStyle $ rectangleSolid cellSize cellSize,
                            color (styleDestroy bgWire) $ rectangleWire cellSize cellSize]
                Orange  -> pictures [ -- draw orange cell
                            color (styleDestroy orange) $ rectangleSolid cellSize cellSize,
                            color (styleDestroy black) $ rectangleWire cellSize cellSize]

                White   -> pictures [ -- draw white cell
                            color (styleDestroy white) $ rectangleSolid cellSize cellSize,
                            color (styleDestroy black) $ rectangleWire cellSize cellSize]
            where 
                getCellObject = cellObject $ fromMaybe emptyCell (getCell grid (x,y))
                getToDestroy = toDestroy $ fromMaybe emptyCell (getCell grid (x,y))

                styleDestroy color -- if toDestroy - remove block outline and darken
                    | color == black && getToDestroy            = transparentColor 
                    | getToDestroy                              = dark color
                    | otherwise                                 = color

                bgColorStyle -- background color is lightened within the grid
                    | y == gridHeight || y == gridHeight - 1    = bgColor 
                    | otherwise                                 = light bgColor
                bgWire -- outline of grid should be invisible at the top two rows
                    | y == gridHeight || y == gridHeight - 1    = bgColor
                    | otherwise                                 = black