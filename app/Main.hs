module Main where

import Prelude hiding (Either, Left,Right)
import Graphics.Gloss
import Data.Int (Int)
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Grid
import Settings
import Game

data Scene = Menu | Game | GameOver
    deriving (Eq, Show)

data World = World {
    scene      :: Scene,        -- current scene of the game
    grid :: Grid,
    controllingCoords :: [Coord],
    controllingCellObjects :: [CellObject],
    direction :: Direction
}

initialWorld = World {
    scene = Game,
    controllingCoords = [(8,12),(9,12),(8,11),(9,11)],
    controllingCellObjects = [Orange,White,Orange,White],
    grid = emptyGrid,
    direction = Stationary
}

draw :: World -> Picture
draw world = drawGrid $ grid world

handler :: Event -> World -> World
handler (EventKey (Char 'a') Down _ _) world =
    case scene world of
        Game        -> world {
            direction = Left
        }
        Menu        -> world
        GameOver    -> world
handler (EventKey (Char 'd') Down _ _) world =
    case scene world of
        Game        -> world {
            direction = Right
        }
        Menu        -> world
        GameOver    -> world

handler (EventKey (Char 'a') Up _ _) world =
    case scene world of
        Game        -> world {
            direction = Stationary
        }
        Menu        -> world
        GameOver    -> world
handler (EventKey (Char 'd') Up _ _) world =
    case scene world of
        Game        -> world {
            direction = Stationary
        }
        Menu        -> world
        GameOver    -> world

handler (EventKey (Char 's') Down _ _) world =
    case scene world of
        Game        -> world {
            controllingCoords = map (\(x,y) -> (x, y - 2)) $ controllingCoords world
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
update time world
    | even (round time)    = world {grid = newGrid, controllingCoords = newControllingCoords}
    | otherwise             = world {grid = newGrid}
    where
        newGrid = makeGrid (fallGrid (grid world)) (controllingCoords world)
        newControllingCoords
            -- if any controllingCoords below top two rows, reset coords ( new block )
            | snd (head (controllingCoords world)) == 12 || snd (head (controllingCoords world)) == 11 =
                case direction world of
                    Left  -> map goLeft $ controllingCoords world
                    Right -> map goRight $ controllingCoords world
                    Stationary -> controllingCoords world
            | otherwise             = [(8,12),(9,12),(8,11),(9,11)]
        goLeft
            | any (\(x,y) -> x == 1) (controllingCoords world)   = \(x,y) -> (x, y)
            | otherwise     = \(x,y) -> (x - 1, y)
        goRight
            | any (\(x,y) -> x == gridWidth) (controllingCoords world)   = \(x,y) -> (x, y)
            | otherwise     = \(x,y) -> (x + 1, y)
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