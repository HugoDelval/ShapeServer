{-# LANGUAGE OverloadedStrings #-}
module Shapes(
  Shape, Point, Vector, Transform, Drawing, StyleSheet, Color, OutlineWidth,
  point, getX, getY, color, outlinewidth, stylesheet, 
  empty, circle, square, getColor, stringFromColor,
  isIdentity, isCompose, isTranslate, isScale, isRotate, get1stComposed, get2ndComposed, getXTranslate, getYTranslate, getXScale, getYScale, getAngleRotate, getInsideColor, getBorderColor, getBorderWidth, isEmpty, isSquare, isCircle,
  identity, translate, rotate, scale, (<+>),
  inside)  where

-- Utilities

data Vector = Vector Double Double
              deriving (Read, Show)
vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c

vectAdd :: Vector -> Vector -> Vector
vectAdd (Vector x1 y1) (Vector x2 y2) = vector (x1+x2) (y1+y2)
        
-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
              deriving (Read, Show)

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

getX (Vector x y) = x
getY (Vector x y) = y

getAngle :: Double -> Double -> Double 
getAngle theCos theSin 
  | angle == asin theSin = degrees angle
  | otherwise = degrees (-angle)
  where angle = acos theCos

degrees :: Double -> Double
degrees x = (x/pi*180)

-- Shapes

type Point  = Vector

point :: Double -> Double -> Point
point = vector


data Shape = Empty 
           | Circle 
           | Square
             deriving (Read, Show)

empty, circle, square :: Shape

empty = Empty
circle = Circle
square = Square

isEmpty :: Shape -> Bool
isEmpty (Empty) = True
isEmpty _ = False

isSquare :: Shape -> Bool
isSquare (Square) = True
isSquare _ = False

isCircle :: Shape -> Bool
isCircle (Circle) = True
isCircle _ = False

-- Transformations

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Compose Transform Transform
           | Rotate Matrix
             deriving (Read, Show)

identity = Identity
translate = Translate
scale = Scale
rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)
t0 <+> t1 = Compose t0 t1

transform :: Transform -> Point -> Point
transform Identity                   x = id x
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
transform (Rotate m)                 p = (invert m) `mult` p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p

isIdentity :: Transform -> Bool
isIdentity (Identity) = True
isIdentity _ = False

isCompose :: Transform -> Bool
isCompose (Compose _ _) = True
isCompose _ = False

isTranslate :: Transform -> Bool
isTranslate (Translate _) = True
isTranslate _ = False

isScale :: Transform -> Bool
isScale (Scale _) = True
isScale _ = False

isRotate :: Transform -> Bool
isRotate (Rotate _) = True
isRotate _ = False

get1stComposed :: Transform -> Transform
get1stComposed (Compose t1 t2) = t1

get2ndComposed :: Transform -> Transform
get2ndComposed (Compose t1 t2) = t2

getXTranslate :: Transform -> Double
getXTranslate (Translate (Vector x y)) = x

getYTranslate :: Transform -> Double
getYTranslate (Translate (Vector x y)) = y

getXScale :: Transform -> Double
getXScale (Scale (Vector x y)) = x

getYScale :: Transform -> Double
getYScale (Scale (Vector x y)) = y

getAngleRotate :: Transform -> Double
getAngleRotate (Rotate (Matrix (Vector tx1 _) (Vector tx2 _))) = getAngle tx1 tx2

-- Drawings

type Drawing = [(Transform,Shape,StyleSheet)]

-- interpretation function for drawings

inside :: Point -> Drawing -> Bool
inside p d = or $ map (inside1 p) d

inside1 :: Point -> (Transform, Shape, StyleSheet) -> Bool
inside1 p (t,s,_) = insides (transform t p) s

insides :: Point -> Shape -> Bool
p `insides` Empty = False
p `insides` Circle = distance p <= 1
p `insides` Square = maxnorm  p <= 1

distance :: Point -> Double
distance (Vector x y ) = sqrt ( x**2 + y**2 )

maxnorm :: Point -> Double
maxnorm (Vector x y ) = max (abs x) (abs y)

testShape = (scale (point 10 10), circle)


-- Stylesheet

data Color = Color String deriving (Read, Show)
color = Color

data OutlineWidth = OutlineWidth Double deriving (Read, Show)
outlinewidth = OutlineWidth

data StyleSheet = StyleSheet Color Color OutlineWidth deriving (Read, Show)
stylesheet = StyleSheet

insideShapeMinusBorder :: Point -> (Shape, Double) -> Bool
p `insideShapeMinusBorder` (Empty, _) = False
p `insideShapeMinusBorder` (Circle, b) = distance p <= 1 - b
p `insideShapeMinusBorder` (Square, b) = maxnorm  p <= 1 - b

getFirstOneInside :: Point -> Drawing -> (Transform,Shape,StyleSheet)
getFirstOneInside p d = head $ filter (inside1 p) d

getColor :: Point -> Drawing -> Color
getColor p d = getColorS p (getFirstOneInside p d)

getColorS :: Point -> (Transform,Shape,StyleSheet) -> Color
getColorS p (t, s, (StyleSheet insideColor borderColor (OutlineWidth borderWidth)))
  | transformedPt `insideShapeMinusBorder` (s, borderWidth) = insideColor
  | otherwise = borderColor
  where transformedPt = (transform t p)

stringFromColor :: Color -> String
stringFromColor (Color c) = c

getInsideColor :: StyleSheet -> String
getInsideColor (StyleSheet insideColor borderColor (OutlineWidth borderWidth)) = stringFromColor insideColor

getBorderColor :: StyleSheet -> String
getBorderColor (StyleSheet insideColor borderColor (OutlineWidth borderWidth)) = stringFromColor borderColor

getBorderWidth :: StyleSheet -> String
getBorderWidth (StyleSheet insideColor borderColor (OutlineWidth borderWidth)) = show borderWidth

