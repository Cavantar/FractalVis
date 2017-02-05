{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Main where

import Graphics.Blank
import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (tryReadTChan)
import Control.Concurrent (threadDelay)

data Vec2D = Vec2D {vx :: Double, vy :: Double} deriving (Eq,Show)
data FractalType = SierpinskiTriangle | KochPath deriving (Eq, Show)

instance Num Vec2D where
  (Vec2D x0 y0) + (Vec2D x1 y1) = Vec2D (x0 + x1) (y0 + y1)
  vec - (Vec2D x1 y1) = vec + (Vec2D (-x1) (-y1))

scaleVec2D :: Vec2D -> Double -> Vec2D
scaleVec2D (Vec2D x y) scalar = Vec2D (x * scalar) (y * scalar)

getLength :: Vec2D -> Double
getLength (Vec2D x y) = ((x ** 2.0) + (y ** 2.0)) ** 0.5

normalize :: Vec2D -> Vec2D
normalize srcVec = scaleVec2D srcVec $ 1 / (getLength srcVec)

getNormal :: Vec2D -> Vec2D
getNormal (Vec2D x y) =
  let orthogonal = Vec2D (-y) x
  in normalize orthogonal

clearCanvas' :: Canvas ()
clearCanvas' = beginPath () >> clearCanvas

drawPointer :: Vec2D -> Canvas ()
drawPointer (Vec2D startX startY) = do
  let boxSide = 10.0
  fillRect (startX - boxSide / 2, startY - boxSide / 2, boxSide, boxSide)

drawTriangle :: Vec2D -> Vec2D -> Vec2D -> Canvas ()
drawTriangle a b c = do
  moveTo (vx a, vy a) >> lineTo (vx b, vy b) >> lineTo (vx c, vy c)
  fillStyle "red" >> fill ()

drawSierpinski :: Vec2D -> Vec2D -> Vec2D -> Int -> Canvas ()
drawSierpinski a b c 0 = drawTriangle a b c
drawSierpinski a b c it = do
  let pA = scaleVec2D (a + b) 0.5
      pB = scaleVec2D (b + c) 0.5
      pC = scaleVec2D (a + c) 0.5
      it' = it - 1
  drawSierpinski a pA pC it'
  drawSierpinski pA b pB it'
  drawSierpinski pC pB c it'

handleMouseMoveSierp :: Vec2D -> Int -> Canvas ()
handleMouseMoveSierp centerPos it = do
  clearCanvas'
  let triangleSide = 400.0
      pA = centerPos + (Vec2D (triangleSide / (-2.0)) (triangleSide / 2.0))
      pB = centerPos + (Vec2D 0 (triangleSide / (-2.0)))
      pC = centerPos + (Vec2D (triangleSide / 2.0) (triangleSide / 2.0))
  drawSierpinski pA pB pC it

drawPath :: [Vec2D] -> Canvas ()
drawPath (startP : pathPoints) = moveTo (vx startP, vy startP) >> mapM_ (\(Vec2D x y) -> lineTo (x,y)) pathPoints >> stroke ()

getKochPath :: Vec2D -> Vec2D -> Int -> [Vec2D]
getKochPath vs ve it =
  let vd = scaleVec2D (ve - vs) 0.25
      p1 = vs + vd
      p2 = vs + (scaleVec2D vd 2) - (scaleVec2D (getNormal vd) (getLength vd))
      p3 = vs + (scaleVec2D vd 3)
  in if it == 0 then
       [vs, p1, p2, p3, ve]
     else
       let it' = it - 1
           seg1 = getKochPath vs p1 it'
           seg2 = getKochPath p1 p2 it'
           seg3 = getKochPath p2 p3 it'
           seg4 = getKochPath p3 ve it'
       in seg1 ++ seg2 ++ seg3 ++ seg4

handleMouseMoveKoch :: Vec2D -> Int -> Canvas ()
handleMouseMoveKoch centerPos it = do
  clearCanvas'
  let lineSegment = 200.0
  drawPath $ getKochPath (centerPos - (Vec2D (lineSegment * 2) 0)) (centerPos + (Vec2D (lineSegment * 2) 0)) it

mainLoop :: Int -> FractalType -> DeviceContext ->IO ()
mainLoop it fractalType context = do
  atomically (tryReadTChan (eventQueue context)) >>= maybe (threadDelay 100000 >> mainLoop it fractalType context) (\event -> do
    (newIt, fractalType') <- case event of
      (Event _ (Just (x,_)) "mousedown" _) -> return $ if width context / 2 < x then (it+1, fractalType) else (it-1, fractalType)
      (Event _ _ "keypress" _)       -> do return (it, if fractalType == SierpinskiTriangle then KochPath else SierpinskiTriangle)
    let centerX = (width context / 2)
        centerY = (height context / 2)
    case fractalType' of
        SierpinskiTriangle -> send context (handleMouseMoveSierp (Vec2D centerX centerY) (abs newIt))
        KochPath           -> send context (handleMouseMoveKoch  (Vec2D centerX centerY) (abs newIt))
    mainLoop newIt fractalType' context
    )

main :: IO ()
main = do
  putStrLn "Starting Application"
  blankCanvas (Options 3000 ["mousedown", "keypress"] False "." [local_only] False) $ mainLoop 1 SierpinskiTriangle
