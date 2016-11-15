{-# LANGUAGE OverloadedStrings #-}
module Main where
-- import Example
-- main = runExample

import Shapes

import Text.Blaze.Svg11 ((!), mkPath, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)

import Web.Scotty

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

ballS :: (Transform, Shape, StyleSheet)
ballS =  ( scale (point 0.7 0.7), circle, blueAndRedBox)

squareS :: (Transform, Shape, StyleSheet)
squareS =  (scale (point 2 2) <+> translate (point 1.2 0.4) <+> rotate 0.39269908169872414, square, blueAndRedBox)

theShapes :: Drawing 
theShapes = [ballS, squareS]

main = do
  let a = renderSvg svgDoc
  putStrLn a
  -- scotty 3000 $ do
  --   get "/" $ do
  --     html "Hello World!"

svgDoc :: S.Svg
svgDoc = S.docTypeSvg ! A.version "1.1" ! A.width "500" ! A.height "500" ! A.viewbox "0 0 10 10" $ do
    foldr1 (>>) (map renderShapeToSVG theShapes)
