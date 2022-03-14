module Main where

import Graphics.Gloss
import Data.Int (Int)
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Grid ( drawGrid, emptyGrid, makeGrid, getCell, Cell (cellState), CellState (Solid), Grid )
import Settings
import Game

data Scene = Menu | Game | GameOver
    deriving (Eq, Show)

data World = World {
    scene      :: Scene,        -- current scene of the game
    grid :: Grid,
    controllingCoords :: [Coord],
    fallingCoords :: [Coord],
    solidCoords :: [Coord]
}

initialWorld = World {
    scene = Game,
    controllingCoords = [(8,11),(9,11),(8,10),(9,10),(8,9),(9,9),(8,8),(9,8)],
    fallingCoords = [(8,11),(9,11),(8,10),(9,10),(8,9),(9,9),(8,8),(9,8)],
    grid = addNewSolid (solidCoords initialWorld) (addNewBlock (fallingCoords initialWorld) emptyGrid),
    solidCoords = [(8,4)]
}

draw :: World -> Picture
draw world = drawGrid $ grid world

handler :: Event -> World -> World
handler (EventKey (Char 'a') Down _ _) world =
    case scene world of
        Game        -> world {
            controllingCoords = map (\(x,y) -> (x - 1, y)) $ controllingCoords world
        }
        Menu        -> world
        GameOver    -> world
handler (EventKey (Char 'd') Down _ _) world =
    case scene world of
        Game        -> world {
            controllingCoords = map (\(x,y) -> (x + 1, y)) $ controllingCoords world
        }
        Menu        -> world
        GameOver    -> world

handler e world = world

{- update :: Float -> World -> World
update time world = world { fallingCoords = newFallingCoords $ fallingCoords world, solidCoords = solidCoords world ++ newSolidCoords (fallingCoords world)}
    where
        newFallingCoords :: [Coord] -> [Coord]
        newFallingCoords [] = []
        newFallingCoords ((x,y):coords)
            | y == 1        = newFallingCoords coords
            | cellState (getCell (makeGrid (fallingCoords world) (solidCoords world)) (x, y-1)) == Solid = newFallingCoords coords
            | otherwise     = (x,y - 1) : newFallingCoords coords

        newSolidCoords :: [Coord] -> [Coord]
        newSolidCoords [] = []
        newSolidCoords ((x,y):coords)
            | y == 1        = (x,y) : newSolidCoords ((x, y + 1):coords)
            | cellState (getCell (makeGrid (fallingCoords world) (solidCoords world)) (x, y-1)) == Solid = (x,y) : newSolidCoords ((x, y + 1):coords)
            | otherwise     = newSolidCoords coords -}

update :: Float -> World -> World
update time world = world {grid = newGrid}
    where
        newGrid = makeGrid (fallGrid (grid world)) (controllingCoords world)

main :: IO ()
main = do
    gamePosition <- getCenterPosition
    play
        (InWindow "Humines" windowSize gamePosition)
        haskellColor
        15
        initialWorld
        draw
        handler
        update