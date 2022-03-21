module Settings where

import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss (makeColorI, Color)
import Data.List (elemIndex)
import Data.Maybe (fromMaybe, isNothing)

type Coord = (Int, Int)

data Direction = Stationary | Left | Right
    deriving (Eq, Show)

haskellColor = makeColorI 94 80 134 255
transparentColor = makeColorI 0 0 0 0
lightTurq = makeColorI 20 100 100 255
darkTurq = makeColorI 30 130 130 255
green = makeColorI 50 130 50 255
darkGreen = makeColorI 34 139 34 255
turq = makeColorI 20 100 120 255

colorList = [haskellColor, green, darkTurq, lightTurq, darkGreen, turq]

-- safely gets the nextColor
getNextColor :: Color -> Maybe Color 
getNextColor currentColor = do
    currentIndex <- elemIndex currentColor colorList
    if 1 + currentIndex >= length colorList
        then Nothing
        else Just (colorList !! (1 + currentIndex))

fps :: Int
fps = 30

timeToDestroy :: Int
timeToDestroy = 5

horizontalSpeed :: Int
horizontalSpeed = fps `div` 15

framesToSeconds :: Int -> Int
framesToSeconds f = f `div` fps

isTimeToDestroy :: Int -> Bool
isTimeToDestroy n
    | n `mod` (timeToDestroy * fps) == 0 && n /= 0  = True
    | otherwise                                     = False

windowSize = (1000,600)

windowWidth = fst windowSize

windowHeight = snd windowSize

getCenterPosition :: IO (Int, Int)
getCenterPosition = do
    -- the center of screen (note: if multiple extended displays - treats it as one whole display)
    (resWidth,resHeight) <- getScreenSize
    pure ((resWidth-windowWidth) `div` 2, (resHeight-windowHeight) `div` 2)