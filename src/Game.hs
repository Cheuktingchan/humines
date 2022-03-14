module Game where

import Settings
import Grid
import Data.Array (array)
import Data.Function ((&))


{- fallColumn :: Coord -> Grid -> Grid
fallColumn (x,y) (Grid oldArr) = setCell  -}

{- fallGrid :: Grid -> Grid
fallGrid grid = Grid (array ((1,1),(gridWidth,gridHeight)) [((x,y), fallCell x y) | x<-[1..gridWidth], y<-[1..gridHeight]])
    where
    fallCell x y
        | y == 1                          = 
            case getCellStateAbove of
                None    -> getCell grid (x,y)
                Solid   -> getCell grid (x,y)
                Falling -> (getCell grid (x,y)) {cellState = Solid , cellObject = cellObject $ getCell grid (x,y + 1)}
        | y == 12                         = getCell grid (x,y)
        | getCellState == None            =
            case getCellStateAbove of
                None    -> getCell grid (x,y)
                Solid   -> getCell grid (x,y)
                Falling -> (getCell grid (x,y)) {cellState = cellState $ getCell grid (x,y + 1) , cellObject = cellObject $ getCell grid (x,y + 1)}
        | getCellState == Solid            =
            case getCellStateAbove of
                None    -> getCell grid (x,y)
                Solid   -> getCell grid (x,y)
                Falling -> getCell grid (x,y)
        | getCellState == Falling         =
            case getCellStateAbove of
                None    -> (getCell grid (x,y)) {cellState = cellState $ getCell grid (x,y + 1) , cellObject = cellObject $ getCell grid (x,y + 1)}
                Solid   -> (getCell grid (x,y)) {cellState = Solid , cellObject = cellObject $ getCell grid (x,y)}
                Falling -> getCell grid (x,y)
        | otherwise                       = getCell grid (x,y)

        where
            getCellState = cellState $ getCell grid (x,y)
            getCellStateAbove = cellState $ getCell grid (x,y + 1)
            getCellStateBelow = cellState $ getCell grid (x,y - 1) -}


fallGrid :: Grid -> Grid
fallGrid grid = foldr fallCell grid coords
    where
        coords :: [Coord]
        coords = [ (x,y) | x <- [gridWidth, gridWidth-1 ..1], y <- [gridHeight , gridHeight -1 ..1]] -- go in reverse because of foldr
        fallCell :: Coord -> Grid -> Grid
        fallCell (x,y) grid = setCell thisCell (x,y) grid
            where
                thisCell :: Cell
                thisCell
                    | y == 1  && getCellState == Falling    =
                        case getCellStateAbove of
                            None    -> getCell grid (x,y)
                            Solid   -> getCell grid (x,y)
                            Falling -> (getCell grid (x,y)) {cellState = Solid , cellObject = cellObject $ getCell grid (x,y)}
                    | y == 1                          =
                        case getCellStateAbove of
                            None    -> getCell grid (x,y)
                            Solid   -> getCell grid (x,y)
                            Falling -> (getCell grid (x,y)) {cellState = Falling , cellObject = cellObject $ getCell grid (x,y + 1)}
                    | y == 12                         = getCell grid (x,y)
                    | getCellState == None            =
                        case getCellStateAbove of
                            None    -> getCell grid (x,y)
                            Solid   -> getCell grid (x,y)
                            Falling -> (getCell grid (x,y)) {cellState = cellState $ getCell grid (x,y + 1) , cellObject = cellObject $ getCell grid (x,y + 1)}
                    | getCellState == Solid            =
                        case getCellStateAbove of
                            None    -> getCell grid (x,y)
                            Solid   -> getCell grid (x,y)
                            Falling -> (getCell grid (x,y)) {cellState = Solid , cellObject = cellObject $ getCell grid (x,y + 1)}
                    | getCellState == Falling && getCellStateAbove == Falling        =
                        case getCellStateBelow of
                            None    -> (getCell grid (x,y)) {cellState = cellState $ getCell grid (x,y + 1) , cellObject = cellObject $ getCell grid (x,y + 1)}
                            Solid   -> (getCell grid (x,y)) {cellState = Solid , cellObject = cellObject $ getCell grid (x,y)}
                            Falling -> getCell grid (x,y)
                    | getCellState == Falling && getCellStateBelow == Solid   =
                        case getCellStateAbove of
                            None    -> (getCell grid (x,y)) {cellState = Solid , cellObject = cellObject $ getCell grid (x,y)}
                            Solid   -> (getCell grid (x,y)) {cellState = Solid , cellObject = cellObject $ getCell grid (x,y + 1)}
                            Falling -> (getCell grid (x,y)) {cellState = Solid , cellObject = cellObject $ getCell grid (x,y + 1)}
                    | getCellState == Falling                                 =
                        case getCellStateAbove of
                            None    -> (getCell grid (x,y)) {cellState = cellState $ getCell grid (x,y + 1) , cellObject = cellObject $ getCell grid (x,y + 1)}
                            Solid   -> (getCell grid (x,y)) {cellState = Solid , cellObject = cellObject $ getCell grid (x,y + 1)}
                            Falling -> (getCell grid (x,y)) {cellState = Solid , cellObject = cellObject $ getCell grid (x,y + 1)}
                    | otherwise                       = getCell grid (x,y)

                    where
                        getCellState = cellState $ getCell grid (x,y)
                        getCellStateAbove = cellState $ getCell grid (x,y + 1)
                        getCellStateBelow = cellState $ getCell grid (x,y - 1)

addNewBlock :: [Coord] -> Grid -> Grid
addNewBlock fallingCoords grid = foldr (setCell Cell {cellObject = Orange, cellState = Falling, controlling = True}) grid fallingCoords

addNewSolid :: [Coord] -> Grid -> Grid
addNewSolid solidCoords grid = foldr (setCell Cell {cellObject = Orange, cellState = Solid, controlling = False}) grid solidCoords