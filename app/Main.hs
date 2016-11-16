{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Example -- Question 1
import Shapes

import Text.Blaze.Svg11 ((!), mkPath, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A
import Text.Blaze.Svg.Renderer.String (renderSvg)
import Data.List

import Web.Scotty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import qualified Text.Blaze.Html.Renderer.Text as R
import Data.Text.Lazy (Text, pack, unpack)
import Control.Exception

import qualified Language.Haskell.Interpreter as HSK

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
greenAndRedBox :: StyleSheet
greenAndRedBox = stylesheet green red borderWidth

ballS :: (Transform, Shape, StyleSheet)
ballS =  ( scale (point 0.7 0.7), circle, blueAndRedBox)
squareS :: (Transform, Shape, StyleSheet)
squareS =  (scale (point 2 2) <+> translate (point 1.2 0.4) <+> rotate 0.39269908169872414, square, greenAndRedBox)

theShapes :: Drawing 
theShapes = [ballS, squareS]

main = do
  -- runExample                  -- Question 1
  -- putStrLn svgDoc theShapes   -- Question 2
  -- Question 3 :
  example <- readFile "sample.shapes"
  scotty 3000 $ do
    get "/" $ do
      html $ home example
    post "/" $ do
      theShapesString <- (param "haskell_DSL") `rescue` (\msg -> return $ mconcat ["Error while processing POST request : ", msg])
      let theShapes :: Drawing
          theShapes = read (unpack theShapesString)
      html $ generateSVG example theShapes

home :: String -> Text 
home example = do
  R.renderHtml $ do
    renderHomePage example

generateSVG :: String -> Drawing -> Text 
generateSVG example theShapes = do
  R.renderHtml $ do
    renderHomePage example
    H.h2 "Here is your SVG:"
    let svg = renderSvg $ svgDoc theShapes
    H.div (H.preEscapedToMarkup svg)

renderHomePage :: String -> H.Html
renderHomePage example = do
  H.head $ H.title "Home Page | Haskell2SVG"
  H.body $ do
    H.h1 "Welcome to Haskell2SVG!"
    H.p "Welcome, using the following form, you can get a SVG representation of your shapes. Try with the default example ;)"
    H.p $ do
      H.p "Note that the shapes are represented as String instances. Here is the representation :"
      H.ul $ do
      	H.li "Drawing = [TransformedShape, TransformedShape, ...]"
      	H.li "TransformedShape = (Transform, Shape, StyleSheet)"
      	H.li "Transform = (Compose(Transform Transform)) | (Scale (Vector)) | (Translate (Vector))) | (Rotate (Vector) (Vector)) "
      	H.li "Vector = (xDouble yDouble) "
      	H.li "Shape = (Empty) | (Square) | (Circle) "
      	H.li "StyleSheet = (Color insideInHexa) (Color borderInHexa) (OutlineWidth borderWidthDouble)) "
    H.p $ do 
      H.form (generateForm example) H.! HA.method "POST"

generateForm :: String -> H.Html
generateForm example = do
  H.label "Your shapes" H.! HA.for "haskell_DSL" >> H.br
  H.textarea (H.toMarkup example) H.! HA.name "haskell_DSL" H.! HA.cols "180" H.! HA.rows "5" >> H.br
  H.input H.! HA.type_ "submit" H.! HA.value "Get SVG!"


-- SVG

svgDoc :: Drawing -> S.Svg
svgDoc theShapes = S.docTypeSvg ! A.version "1.1" ! A.width "500" ! A.height "500" ! A.viewbox "0 0 10 10" $ do
    foldr1 (>>) (map renderShapeToSVG theShapes)

renderShapeToSVG :: (Transform, Shape, StyleSheet) -> S.Svg
renderShapeToSVG (t, s, ss) = S.g ! A.transform (S.translate 5 5) $ do
  S.g ! A.transform (mconcat (makeTransform t)) $ do
    renderShape s ss

makeTransform :: Transform -> [S.AttributeValue]
makeTransform transform 
  | isIdentity transform = []
  | isCompose transform = makeTransform (get1stComposed transform) ++ makeTransform (get2ndComposed transform)
  | isTranslate transform = [S.translate (getXTranslate transform) (getYTranslate transform)]
  | isScale transform = [S.scale (getXScale transform) (getYScale transform)]
  | isRotate transform = [S.translate 0.5 0.5, S.rotate (getAngleRotate transform), S.translate (-0.5) (-0.5)]

renderShape :: Shape -> StyleSheet -> S.Svg
renderShape shape stylesheet
  | isEmpty shape = S.rect ! A.width "0" ! A.height "0"
  | isSquare shape = do
      S.rect ! A.width "1" ! A.height "1" ! 
        A.fill (S.stringValue ("#" ++ insideColor)) ! 
        A.stroke (S.stringValue ("#" ++ borderColor)) ! A.strokeWidth (S.stringValue borderWidth)
  | isCircle shape = do
      S.circle ! A.r "0.5" !
        A.fill (S.stringValue ("#" ++ insideColor)) ! 
        A.stroke (S.stringValue ("#" ++ borderColor)) ! A.strokeWidth (S.stringValue borderWidth)
  where insideColor = getInsideColor stylesheet
        borderColor = getBorderColor stylesheet
        borderWidth = getBorderWidth stylesheet
