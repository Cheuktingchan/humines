module Settings where

import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss (makeColorI)

type Coord = (Int, Int)

haskellColor = makeColorI 94 80 134 255

windowSize = (1000,600)

windowWidth = fst windowSize

windowHeight = snd windowSize

getCenterPosition :: IO (Int, Int)
getCenterPosition = do
    (resWidth,resHeight) <- getScreenSize   -- places window in the center of screen (if multiple extended displays - treats as one whole display)
    pure ((resWidth-windowWidth) `div` 2, (resHeight-windowHeight) `div` 2)