module Game where

import Settings
import Grid
import Data.Array (array)
import Data.Function ((&))

fallGrid :: Grid -> Grid
fallGrid grid = foldr fallCell grid coords
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
                    -- Top two rows of the grid are for dropping (don't fall)
                    | y == 12 || y == 11                                = Cell {cellState = None , cellObject = Empty, controlling = False}
                    | y == 1  && getCellState == Falling                =
                        case getCellStateAbove of
                            None    -> thisCell
                            Solid   -> thisCell
                            Falling -> thisCell {cellState = Solid , cellObject = cellObject thisCell}
                    | (y == 1  || getCellState == None) && y /= 10      =
                        case getCellStateAbove of
                            None    -> thisCell
                            Solid   -> thisCell
                            Falling -> thisCell {cellState = Falling , cellObject = cellObject cellAbove}
                    | getCellState == Solid                             =
                        case getCellStateAbove of
                            None    -> thisCell
                            Solid   -> thisCell
                            Falling -> thisCell {cellState = Solid , cellObject = cellObject cellAbove}
                    | getCellState == Falling && getCellStateAbove == Falling        =
                        case getCellStateBelow of
                            None    -> thisCell {cellState = cellState cellAbove , cellObject = cellObject cellAbove}
                            Solid   -> thisCell {cellState = Solid , cellObject = cellObject thisCell}
                            Falling -> thisCell
                    | getCellState == Falling && getCellStateBelow == Solid   =
                        case getCellStateAbove of
                            None    -> thisCell {cellState = Solid , cellObject = cellObject thisCell}
                            Solid   -> thisCell {cellState = Solid , cellObject = cellObject cellAbove}
                            Falling -> thisCell {cellState = Solid , cellObject = cellObject cellAbove}
                    | getCellState == Falling                                 =
                        case getCellStateAbove of
                            None    -> thisCell {cellState = None , cellObject = cellObject cellAbove}
                            Solid   -> thisCell {cellState = Solid , cellObject = cellObject cellAbove}
                            Falling -> thisCell {cellState = Solid , cellObject = cellObject cellAbove}
                    | otherwise                       = thisCell

                    where
                        thisCell = getCell grid (x,y)
                        cellAbove = getCell grid (x,y + 1)
                        cellBelow = getCell grid (x,y - 1)
                        getCellState = cellState $ getCell grid (x,y)
                        getCellStateAbove = cellState $ getCell grid (x,y + 1)
                        getCellStateBelow = cellState $ getCell grid (x,y - 1)


addNewBlock :: [Coord] -> Grid -> Grid
addNewBlock fallingCoords grid = foldr (setCell Cell {cellObject = Orange, cellState = Falling, controlling = True}) grid fallingCoords

addNewSolid :: [Coord] -> Grid -> Grid
addNewSolid solidCoords grid = foldr (setCell Cell {cellObject = Orange, cellState = Solid, controlling = False}) grid solidCoords