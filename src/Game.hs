module Game where

import Settings
import Grid
import Data.Array (array)
import Data.Maybe (fromMaybe)

allPossibleBlocks :: [[CellObject]]
allPossibleBlocks = sequenceA (replicate 4 [Orange,White])

-- updateGrid calls setCell n times where n is the size of the grid. This is neccessary because the logic for each cell
-- requires the information of the cells before it to be updated already.
updateGrid :: Bool -> Grid -> Grid
updateGrid destroyBlocks grid = foldr updateCell grid coords
    where
        -- this ordering of the coordinates  for updating grid succesively left to right, top to bottom
        coords :: [Coord]
        coords = [ (x,y) | x <- [gridWidth, gridWidth-1 ..1], y <- [gridHeight , gridHeight -1 ..1]]

        -- sets the grid for these cell coordinates
        updateCell :: Coord -> Grid -> Grid
        updateCell (x,y) grid = setCell thisCoordCell (x,y) grid
            where
                -- rules for updating cells for the next frame in the game
                thisCoordCell :: Cell
                thisCoordCell
                    -- if blocks are supposed to be destroyed this frame destroy them
                    | destroyBlocks && toDestroy thisCell                       = Cell {cellState = None , cellObject = Empty, toDestroy = False}
                    -- if blocks have been destroyed the above have to fall successively
                    | destroyBlocks && getCellState == Solid                    = 
                        case getCellStateBelow of
                            None    -> thisCell {cellState = Falling}
                            Solid   -> thisCell
                            Falling -> thisCell {cellState = Falling}
                    -- if solid and not marked destroy already check and mark if it is eligible to destroy
                    | getCellState == Solid && not (toDestroy thisCell)         = setDestroyCell thisCell
                    -- top two rows need to be clear for dropping
                    | y == gridHeight || y == gridHeight - 1                    = Cell {cellState = None , cellObject = Empty, toDestroy = False}
                    -- if at the bottom row and still falling, turn it to solid
                    | y == 1  && getCellState == Falling                        =
                        case getCellStateAbove of
                            Falling -> thisCell {cellState = Solid}
                            _       -> thisCell
                    -- if at (bottom row or cell is nothing) and not top non drop row
                    | (y == 1  || getCellState == None) && y /= gridHeight - 2  =
                        case getCellStateAbove of
                            Falling -> thisCell {cellState = Falling , cellObject = cellObject cellAbove} -- set this cell object to the one above
                            _       -> thisCell
                    -- if cell is falling and above is also falling
                    | getCellState == Falling && getCellStateAbove == Falling   =
                        case getCellStateBelow of -- check cell below:
                            Solid   -> thisCell {cellState = Solid} -- if below solid, this cell needs to be solid
                            _       -> thisCell {cellObject = cellObject cellAbove} -- set object to the one above
                    -- if cell is falling and below is solid (but above is not falling (by precondition))
                    | getCellState == Falling && getCellStateBelow == Solid     =
                        case getCellStateAbove of
                            None    -> thisCell {cellState = Solid} -- if nothing above, stop falling
                            _       -> thisCell {cellState = Solid , cellObject = cellObject cellAbove} -- otherwise still fall
                    -- if cell is falling and are not the edge cases covered above
                    | getCellState == Falling                                   = thisCell {cellState = getCellStateAbove, cellObject = cellObject cellAbove}
                    -- otherwise keep the same
                    | otherwise                                                 = thisCell

                    where
                        -- shorthand getters for readability within the rules above
                        thisCell = fromMaybe emptyCell (getCell grid (x,y))
                        cellAbove
                            | y == gridHeight - 2   = Cell {cellState = None, cellObject = Empty, toDestroy = False}
                            | otherwise             = fromMaybe emptyCell (getCell grid (x,y + 1))
                        cellBelow = getCell grid (x,y - 1)
                        getCellState = cellState $ fromMaybe emptyCell (getCell grid (x,y))
                        getCellStateAbove = cellState $ fromMaybe emptyCell (getCell grid (x,y + 1))
                        getCellStateBelow = cellState $ fromMaybe emptyCell (getCell grid (x,y - 1))

                        -- logic for checking if block can be destroyed
                        solidSquareCheck :: Coord -> Bool
                        solidSquareCheck (x,y) = and [cellState z == Solid && cellObject z == cellObject thisCell | z <- fromMaybe emptyCell . getCell grid <$> [(x,y), (x,y + 1), (x + 1, y), (x + 1, y + 1)]]

                        setDestroyCell :: Cell -> Cell
                        setDestroyCell cell -- just check to the right because we loop right to left already
                            | or [ a | a <- solidSquareCheck <$> [(x - 1, y - 1), (x - 1, y), (x , y - 1), (x, y)]] = cell {toDestroy = True}
                            | otherwise                                                                             = cell

-- get the number of cells marked for destroying to keep track of score
numberToDestroy :: Grid -> Int 
numberToDestroy grid = isDestroy coords
    where
        coords :: [Coord] -- check the whole grid
        coords = [ (x,y) | x <- [gridWidth, gridWidth-1 ..1], y <- [gridHeight , gridHeight -1 ..1]]

        -- recursively add 1 to result if is marked for destroy
        isDestroy :: [Coord] -> Int
        isDestroy [] = 0
        isDestroy ((x,y):coords)
            | toDestroy $ fromMaybe emptyCell (getCell grid (x,y)) = 1 + isDestroy coords
            | otherwise                                          = 0 + isDestroy coords

