module Grid where

import Data.Array
import Graphics.Gloss
import Settings
import Data.Function ((&))
import Data.Maybe ( fromMaybe )

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

getCell :: Grid -> Coord -> Maybe Cell
getCell (Grid gridArr) (x,y)
    | x > gridWidth || x < 1 || y > gridHeight || y < 1 = Nothing
    | otherwise                                         = Just (gridArr ! (x,y))

setCell :: Cell -> Coord -> Grid -> Grid
setCell cell coord (Grid gridArr) = Grid $ gridArr // [(coord,cell)]

gridWidth = 16
gridHeight = 12   -- 10 proper rows but we allocate the top two rows for dropping space

emptyCell :: Cell
emptyCell = Cell {cellObject = Empty, cellState = None, toDestroy = False}

emptyGrid :: Grid
emptyGrid = Grid (array ((1,1),(gridWidth,gridHeight)) [((x,y), emptyCell) | x<-[1..gridWidth], y<-[1..gridHeight]])

{- makeGrid :: Grid -> [Coord] -> Grid
makeGrid = foldr (setCell Cell {cellObject = Orange, cellState = Falling,}) -}

makeGrid :: [CellObject] -> [Coord] -> Grid -> Grid -- is there an implicit way to do this? zip?
makeGrid [] [] grid = grid
{- makeGrid [] _ grid = grid
makeGrid _ [] grid = grid -}
makeGrid (object:objects) (coord:coords) grid = makeGrid objects coords (setCell Cell {cellObject = object, cellState = Falling, toDestroy = False} coord grid)

drawGrid :: Grid -> Picture
drawGrid grid = pictures [drawCell x y | x <- [1..gridWidth], y <- [1..gridHeight]]
    where
        drawCell x y = translate (fromIntegral x * cellSize - fromIntegral (windowWidth `div` 2 - gridWidth * 10)) (fromIntegral y * 40 - fromIntegral (windowHeight `div` 2 - gridHeight * 6)) $

            case getCellObject of
                Empty   -> pictures [
                            color haskellColor $ rectangleSolid cellSize cellSize,
                            color (styleDestroy black) $ rectangleWire cellSize cellSize]
                Orange  -> pictures [
                            color (styleDestroy orange) $ rectangleSolid cellSize cellSize,
                            color (styleDestroy black) $ rectangleWire cellSize cellSize]

                White   -> pictures [
                            color (styleDestroy white) $ rectangleSolid cellSize cellSize,
                            color (styleDestroy black) $ rectangleWire cellSize cellSize]
{-             case getCellState of
                None   -> pictures [
                            color haskellColor $ rectangleSolid cellSize cellSize,
                            color black $ rectangleWire cellSize cellSize]
                Falling  -> color orange $ rectangleSolid cellSize cellSize

                Solid   -> color white $ rectangleSolid cellSize cellSize -}
                where
                    getCellObject = cellObject $ fromMaybe emptyCell (getCell grid (x,y))
                    getCellState = cellState $ fromMaybe emptyCell (getCell grid (x,y))
                    getToDestroy = toDestroy $ fromMaybe emptyCell (getCell grid (x,y))

                    styleDestroy color -- if toDestroy - remove block outline and darken
                        | color == black && getToDestroy    = transparentColor 
                        | getToDestroy                      = dark color
                        | otherwise                         = color