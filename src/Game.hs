module Game where

import Settings
import Grid
import Data.Array (array)
import Data.Function ((&))
import Data.Maybe (fromMaybe)

allPossibleBlocks :: [[CellObject]]
allPossibleBlocks = sequenceA (replicate 4 [Orange,White])

updateGrid :: Grid -> Grid
updateGrid grid = foldr fallCell grid coords
    where
        -- this ordering for updating grid succesively left to right, top to bottom
        coords :: [Coord]
        coords = [ (x,y) | x <- [gridWidth, gridWidth-1 ..1], y <- [gridHeight , gridHeight -1 ..1]]

        fallCell :: Coord -> Grid -> Grid
        fallCell (x,y) grid = setCell thisCoordCell (x,y) grid
            where
                -- rules for falling cells in the game
                thisCoordCell :: Cell
                thisCoordCell
                    -- Top two rows of the grid are for dropping (usual logic does not apply)
                    | getCellState == Solid                                     = setDestroyCell thisCell
                    | y == gridHeight || y == gridHeight - 1                    = Cell {cellState = None , cellObject = Empty, toDestroy = False}
                    | y == 1  && getCellState == Falling                        =
                        case getCellStateAbove of
                            None    -> thisCell
                            Solid   -> thisCell
                            Falling -> thisCell {cellState = Solid , cellObject = cellObject thisCell}
                    | (y == 1  || getCellState == None) && y /= gridHeight - 2  =
                        case getCellStateAbove of
                            None    -> thisCell
                            Solid   -> thisCell
                            Falling -> thisCell {cellState = Falling , cellObject = cellObject cellAbove}
                    | getCellState == Falling && getCellStateAbove == Falling   =
                        case getCellStateBelow of
                            None    -> thisCell {cellState = cellState cellAbove , cellObject = cellObject thisCell}
                            Solid   -> thisCell {cellState = Solid , cellObject = cellObject thisCell}
                            Falling -> thisCell {cellState = cellState cellAbove , cellObject = cellObject cellAbove}
                    | getCellState == Falling && getCellStateBelow == Solid     =
                        case getCellStateAbove of
                            None    -> thisCell {cellState = Solid , cellObject = cellObject thisCell}
                            Solid   -> thisCell {cellState = Solid , cellObject = cellObject cellAbove}
                            Falling -> thisCell {cellState = Solid , cellObject = cellObject cellAbove}
                    | getCellState == Falling                                   =
                        case getCellStateAbove of
                            None    -> thisCell {cellState = None , cellObject = cellObject cellAbove}
                            Solid   -> thisCell {cellState = Solid , cellObject = cellObject cellAbove}
                            Falling -> thisCell {cellState = Falling , cellObject = cellObject cellAbove}
                    | otherwise                                                 = thisCell

                    where
                        thisCell = fromMaybe emptyCell (getCell grid (x,y))
                        cellAbove
                            | y == gridHeight - 2   = Cell {cellState = None, cellObject = Empty, toDestroy = False}
                            | otherwise             = fromMaybe emptyCell (getCell grid (x,y + 1))
                        cellBelow = getCell grid (x,y - 1)
                        getCellState = cellState $ fromMaybe emptyCell (getCell grid (x,y))
                        getCellStateAbove = cellState $ fromMaybe emptyCell (getCell grid (x,y + 1))
                        getCellStateBelow = cellState $ fromMaybe emptyCell (getCell grid (x,y - 1))

                        solidSquareCheck :: Coord -> Bool
                        solidSquareCheck (x,y) = and [cellState z == Solid && cellObject z == cellObject thisCell | z <- fromMaybe emptyCell . getCell grid <$> [(x,y), (x,y + 1), (x + 1, y), (x + 1, y + 1)]]

                        setDestroyCell :: Cell -> Cell
                        setDestroyCell cell -- just check to the right because we loop right to left already
                            | or [a | a <- solidSquareCheck <$> [(x - 1, y - 1), (x - 1,y), (x , y - 1), (x,y)]]  = cell {toDestroy = True}
                            | otherwise                                             = cell


addNewBlock :: [Coord] -> Grid -> Grid
addNewBlock fallingCoords grid = foldr (setCell Cell {cellObject = Orange, cellState = Falling, toDestroy = False}) grid fallingCoords

addNewSolid :: [Coord] -> Grid -> Grid
addNewSolid solidCoords grid = foldr (setCell Cell {cellObject = Orange, cellState = Solid, toDestroy = False}) grid solidCoords

