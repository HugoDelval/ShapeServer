module Ansi where

import Shapes (Color, color, stringFromColor)

data Colour = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  deriving (Eq,Show,Enum)

cls :: IO ()
cls = putStr "\ESC[2J"

colourFromHexaColor :: Color -> Colour
colourFromHexaColor c
	| sc == "00ff00" = Green
	| sc == "ff0000" = Red
	| sc == "0000ff" = Blue
	where sc = stringFromColor c

goto :: Int -> Int -> IO ()
goto x y    = putStr $ "\ESC[" ++ show y ++ ";" ++ show x ++ "H"

color_ansi :: Color -> String -> IO ()
color_ansi c s = putStr $ "\ESC[3" ++ show (fromEnum $ colourFromHexaColor c) ++ "m" ++ s ++ "\ESC[0m"

