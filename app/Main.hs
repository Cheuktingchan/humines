module Main where

import Prelude hiding (Either, Left,Right)
import System.Random ( mkStdGen, Random(randomR), StdGen )
import Graphics.Gloss
import Data.Int (Int)
import Graphics.Gloss.Interface.IO.Interact
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Grid
import Settings
import Game
import Data.Maybe (fromMaybe)

data Scene = Menu | Game | GameOver
    deriving (Eq, Show)

data World = World {
    scene      :: Scene,                    -- stage of the game
    grid :: Grid,                           -- grid
    controllingCoords :: [Coord],           -- controlled block coordinates
    controllingCellObjects :: [CellObject], -- controlled block object types
    direction :: Direction,                 -- direction block is moving in
    randomSeed :: (Int, StdGen),            -- the seed generated for random blocks
    frameCount :: Int,                      -- the number of frames since the current scene started
    score :: Int,                           -- the score during Game scenes                         
    bgColor :: Color                        -- the background color
}

initialWorld = World {
    scene = Menu,
    controllingCoords = [(8,12),(9,12),(8,11),(9,11)],
    controllingCellObjects = [Orange,White,Orange,White],
    grid = emptyGrid,
    direction = Stationary,
    randomSeed = randomR (0, 15) (mkStdGen 1),
    frameCount = 0,
    score = 0,
    bgColor = haskellColor
}

draw :: World -> Picture
draw world =
    case scene world of
        Menu -> pictures $
        -- drawing the menu game title and instructions
            [translate (fromIntegral (-windowWidth `div` 5)) 100 $ scale 0.5 0.5 $ text "Humines"]
            ++
            [translate (fromIntegral (-windowWidth `div` 5)) 0 $ scale 0.25 0.25 $ text "Press W to play!"]
            ++
            [translate (fromIntegral (-windowWidth `div` 5)) (-100) $ scale 0.1 0.1 $ text "A : Go left"]
            ++
            [translate (fromIntegral (-windowWidth `div` 5)) (-150) $ scale 0.1 0.1 $ text "A : Go right"]
            ++
            [translate (fromIntegral (-windowWidth `div` 5)) (-200) $ scale 0.1 0.1 $ text "Space : Drop block"]

        Game -> pictures $ 
        -- draw the background color
            [color (bgColor world) $ rectangleSolid (fromIntegral windowWidth) (fromIntegral windowHeight)]
            ++
        -- draw the gird
            [drawGrid (bgColor world) (grid world)]
            ++
        -- draw the UI (Destory timer, Time left, Score)
            [translate (fromIntegral (-windowWidth `div` 2) + 10) 100 $ scale 0.25 0.25 $ text "Destroy:"]
            ++
            [translate (fromIntegral (-windowWidth `div` 2) + 10) 60 $ scale 0.25 0.25 $ text $ show $ 5 - framesToSeconds (frameCount world) `mod` timeToDestroy]
            ++
            [translate (fromIntegral (-windowWidth `div` 2) + 10) (-100) $ scale 0.25 0.25 $ text "Time left:"]
            ++
            [translate (fromIntegral (-windowWidth `div` 2) + 10) (-140) $ scale 0.25 0.25 $ text $ show $ 30 - framesToSeconds (frameCount world)]
            ++
            [translate (fromIntegral (windowWidth `div` 3) + 10) 100 $ scale 0.25 0.25 $ text "Score:"]
            ++
            [translate (fromIntegral (windowWidth `div` 3) + 10) 60 $ scale 0.25 0.25 $ text $ show $ score world]
        GameOver -> pictures $
        -- draw game over screen and instructions
            [translate (fromIntegral (-windowWidth `div` 5)) 100 $ scale 0.5 0.5 $ text "Finished!"]
            ++
            [translate (fromIntegral (-windowWidth `div` 5)) 0 $ scale 0.25 0.25 $ text $ "You Got: " ++ show (score world)]
            ++
            [translate (fromIntegral (-windowWidth `div` 5)) (-50) $ scale 0.25 0.25 $ text "Press W to play again!"]

handler :: Event -> World -> World
-- keys pressed down events
handler (EventKey (Char char) Down _ _) world = 
    case char of 
        'a' -> 
            case scene world of
                Game        -> world {direction = Left}
                _           -> world 
        'w' -> 
            case scene world of
                Menu        -> world {
                    scene = Game,
                    frameCount = 0
                }
                GameOver    -> initialWorld{
                    scene = Game,
                    randomSeed = randomSeed world
                }
                Game        -> world
        'd' -> 
            case scene world of
                Game        -> world {
                    direction = Right
                }
                _        -> world
        _   -> world

-- key released events
handler (EventKey (Char char) Up _ _) world =
    case char of
        'a' -> 
            case scene world of
                Game        -> world {
                    direction = if direction world == Left then Stationary else Right
                }
                _        -> world
        'd' -> 
            case scene world of
                Game        -> world {
                    direction = if direction world == Right then Stationary else Left
                }
                _        -> world
        _   -> world

-- space pressed down event
handler (EventKey (SpecialKey KeySpace) Down _ _) world =
    case scene world of
        Game        -> world {
            controllingCoords = map (\(x,y) -> (x, y - 2)) $ controllingCoords world
        }
        _           -> world

handler e world = world

update :: Float -> World -> World
update time world = world {
    scene = newScene,
    grid = newGrid,
    controllingCoords = newControllingCoords,
    randomSeed = newSeed,
    controllingCellObjects = allPossibleBlocks !! fst newSeed,
    frameCount = frameCount world + 1,
    score = score world + newlyDestroyed,
    bgColor = newBgColor
    }
    where
        -- check if game over
        newScene
            | gameIsOver        = GameOver
            | otherwise         = scene world
        gameIsOver
            | frameCount world > (30 * fps) = True
            | otherwise                     = or [cellState z == Solid | z <- fromMaybe emptyCell . getCell (grid world) <$> [(x,gridHeight - 2) | x <- [1 .. gridWidth]]]
        
        -- get new grid which depending on whether it is time to destroy
        newGrid
            | isTimeToDestroy (frameCount world)    = makeGrid (controllingCellObjects world) (controllingCoords world) (updateGrid True (grid world))
            | otherwise                             = makeGrid (controllingCellObjects world) (controllingCoords world) (updateGrid False (grid world))
        
        -- get new coordinates of the controlling block
        newControllingCoords -- if the controllingCoords are not in rows 11 or 12, reset coords ( new block )
            | snd (head (controllingCoords world)) == 12 || snd (head (controllingCoords world)) == 11 =
                case direction world of
                    Left  -> map goLeft $ controllingCoords world
                    Right -> map goRight $ controllingCoords world
                    Stationary -> controllingCoords world
            | otherwise             = [(8,12),(9,12),(8,11),(9,11)]
        goLeft -- if any block in column 1, don't go left any more, else go left
            | all (\(x,y) -> x /= 1) (controllingCoords world) && frameCount world `mod` horizontalSpeed == 0          = \(x,y) -> (x - 1, y)
            | otherwise     = \(x,y) -> (x, y)
        goRight -- if any block in column gridWidth, don't go right any more, else go right
            | all (\(x,y) -> x /= gridWidth) (controllingCoords world) && frameCount world `mod` horizontalSpeed == 0  = \(x,y) -> (x + 1, y)
            | otherwise     = \(x,y) -> (x, y)

        -- add this number to the score if it is time to destroy
        newlyDestroyed 
            | isTimeToDestroy (frameCount world)    = numberToDestroy $ grid world
            | otherwise                             = 0
        
        -- generate a new seed if block gets placed for the next block to be random
        newSeed :: (Int, StdGen)
        newSeed
            | snd (head (controllingCoords world)) == 12 || snd (head (controllingCoords world)) == 11 = randomSeed world
            | otherwise             = randomR (0, 15) (snd $ randomSeed world)
        
        -- if time to destroy change background color to the next in colorList
        newBgColor
            -- by defaulting to haskellColor it will loop back through the list of colors if Nothing
            | isTimeToDestroy (frameCount world)    = fromMaybe haskellColor $ getNextColor $ bgColor world
            | otherwise                             = bgColor world

main :: IO ()
main = do
    gamePosition <- getCenterPosition
    play
        (InWindow "Humines" windowSize gamePosition)
        haskellColor
        fps
        initialWorld
        draw
        handler
        update