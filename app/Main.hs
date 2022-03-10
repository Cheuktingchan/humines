module Main where

import Grid
import Graphics.Gloss
import Data.Int (Int)
import Graphics.Gloss.Interface.IO.Interact (Event (..), Key (..), SpecialKey (..) )
import Graphics.Gloss.Interface.Environment (getScreenSize)

data Scene = Menu | Game | GameOver
    deriving (Eq, Show)

data World = World {
    scene      :: Scene        -- Is the game running?
}

makeWorld = World {
    scene = Menu
}


windowSize = (1000,600)

windowWidth = fst windowSize
windowHeight = snd windowSize

getCenterPosition :: IO (Int, Int)
getCenterPosition = do
    (resWidth,resHeight) <- getScreenSize   -- places window in the center of screen (if multiple extended displays - treats as one whole display)
    pure ((resWidth-windowWidth) `div` 2, (resHeight-windowHeight) `div` 2)
    
draw :: World -> Picture
draw world = pictures [drawGrid emptyGrid]

handleInput :: Event -> World -> World
handleInput e world = world

update :: Float -> World -> World
update time world = world

main :: IO ()
main =
    do
    gamePosition <- getCenterPosition
    play (InWindow "Humines" windowSize gamePosition) backgroundColor 60 makeWorld draw handleInput update
