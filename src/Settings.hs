module Settings where

import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss (makeColorI)

type Coord = (Int, Int)

data Direction = Stationary | Left | Right
    deriving (Eq, Show)

haskellColor = makeColorI 94 80 134 255
transparentColor = makeColorI 0 0 0 0


crayolaColor = makeColorI 196 91 170 255
windowSize = (1000,600)

windowWidth = fst windowSize

windowHeight = snd windowSize

getCenterPosition :: IO (Int, Int)
getCenterPosition = do
    -- the center of screen (note: if multiple extended displays - treats it as one whole display)
    (resWidth,resHeight) <- getScreenSize
    pure ((resWidth-windowWidth) `div` 2, (resHeight-windowHeight) `div` 2)