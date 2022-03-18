module Main where

import Prelude hiding (Either, Left,Right)
import System.Random
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
    direction :: Direction,
    randomSeed :: (Int, StdGen),
    frameCount :: Int,
    score :: Int,
    timer :: Int
}

initialWorld = World {
    scene = Game,
    controllingCoords = [(8,12),(9,12),(8,11),(9,11)],
    controllingCellObjects = [Orange,White,Orange,White],
    grid = emptyGrid,
    direction = Stationary,
    randomSeed = randomR (0, 15) (mkStdGen 1),
    frameCount = 0,
    score = 0,
    timer = 0
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
            direction = if direction world == Left then Stationary else Right
        }
        Menu        -> world
        GameOver    -> world

handler (EventKey (Char 'd') Up _ _) world =
    case scene world of
        Game        -> world {
            direction = if direction world == Right then Stationary else Left
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
update time world = world {grid = newGrid,
    controllingCoords = newControllingCoords,
    randomSeed = newSeed,
    controllingCellObjects = allPossibleBlocks !! fst newSeed,
    frameCount = frameCount world + 1}
    where
        newGrid 
            | frameCount world `mod` 360 == 0   = makeGrid (controllingCellObjects world) (controllingCoords world) (updateGrid True (grid world)) 
            | otherwise                         = makeGrid (controllingCellObjects world) (controllingCoords world) (updateGrid False (grid world))
        newControllingCoords -- if the controllingCoords are not in rows 11 or 12, reset coords ( new block )
            | snd (head (controllingCoords world)) == 12 || snd (head (controllingCoords world)) == 11 =
                case direction world of
                    Left  -> map goLeft $ controllingCoords world
                    Right -> map goRight $ controllingCoords world
                    Stationary -> controllingCoords world
            | otherwise             = [(8,12),(9,12),(8,11),(9,11)]
        goLeft -- if any block in column 1, don't go left any more, else go left
            | all (\(x,y) -> x /= 1) (controllingCoords world) && frameCount world `mod` 5 == 0          = \(x,y) -> (x - 1, y)
            | otherwise     = \(x,y) -> (x, y)
        goRight -- if any block in column gridWidth, don't go right any more, else go right
            | all (\(x,y) -> x /= gridWidth) (controllingCoords world) && frameCount world `mod` 5 == 0  = \(x,y) -> (x + 1, y)
            | otherwise     = \(x,y) -> (x, y)

        newSeed :: (Int, StdGen)
        newSeed
            | snd (head (controllingCoords world)) == 12 || snd (head (controllingCoords world)) == 11 = randomSeed world
            | otherwise             = randomR (0, 15) (snd $ randomSeed world)

main :: IO ()
main = do
    gamePosition <- getCenterPosition
    play
        (InWindow "Humines" windowSize gamePosition)
        haskellColor
        60
        initialWorld
        draw
        handler
        update