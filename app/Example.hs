module Example where

import Shapes
import Signal
import Animate (animate)
import Render (defaultWindow)

staticBall :: Signal Drawing
staticBall = pure [(scale (point 0.5 0.5) <+> translate (point 1.2 0.4), circle, blueAndRedBox)]

addT :: (Transform, Shape, StyleSheet) -> Transform -> (Transform, Shape, StyleSheet)
addT (ts,s,ss) t = (ts <+> t, s, ss)

preaddT :: (Transform, Shape, StyleSheet) -> Transform -> (Transform, Shape, StyleSheet)
preaddT (ts,s,ss) t = (t <+> ts, s, ss)

rotatingSquare :: Signal Drawing
rotatingSquare = fmap (:[]) $ fmap (addT sq) rs 
     where            
           rs :: Signal Transform
           rs = fmap rotate timeS -- using timeS as the source for the rotation angle

           sq :: (Transform, Shape, StyleSheet)
           sq = ( scale (point 0.5 0.5) <+> translate (point 1.2 0.4) , square, blueAndRedBox)

bouncingBall :: Signal Drawing
bouncingBall = fmap (:[]) $ fmap (preaddT ball) ( fmap translate pos )
       where bounceY = fmap (sin . (3*)) timeS
             bounceX = fmap (sin . (2*)) timeS
             pos = pure point <*> bounceX <*> bounceY
             ball = ( scale (point 0.7 0.7), circle, blueAndRedBox)

red :: Color
red = color "ff0000"
green :: Color
green = color "00ff00"
blue :: Color
blue = color "0000ff"
borderWidth :: OutlineWidth
borderWidth = outlinewidth 0.1

blueAndRedBox :: StyleSheet
blueAndRedBox = stylesheet blue red borderWidth

movingBall :: Signal Drawing
movingBall = fmap (:[]) $ fmap (addT ball) ts
       where
             ts :: Signal Transform
             ts = fmap translate pos

             bounceY :: Signal Double
             bounceY = fmap (sin . (3*)) timeS

             pos :: Signal Point
             pos = pure point <*> pure 0.0 <*> bounceY

             ball :: (Transform, Shape, StyleSheet)
             ball = ( scale (point 0.3 0.3), circle, blueAndRedBox)

joinDS :: Signal [a] -> Signal [a] -> Signal [a]
joinDS s0 s1 = (fmap ( (++) ) s0) <*> s1


--example = staticBall
example = bouncingBall `joinDS` rotatingSquare

           
runExample :: IO ()
runExample = animate defaultWindow 0 endTime example
  where endTime = 15

