module Cube where

import Prelude
import Control.Monad.Eff
import Control.Monad.ST
import Data.Maybe
import Math
import Global.Unsafe
import Data.Int
import Data.Traversable (for)
import DOM
import Data.Array
import Data.Array.ST
import Graphics.Canvas
import Partial.Unsafe (unsafePartial)
import Control.Monad.Eff.Console
import Data.Foldable (for_)
import Control.Monad.Except (runExcept)
import Data.Foreign (readString)
import Control.Monad.Eff.Ref
import DOM.HTML (window)
import DOM.HTML.Types (Window())
import DOM (DOM())
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import DOM.RequestAnimationFrame
import Element as J

qx = pi/4.0
qy = pi/3.0
qz = pi/4.0

mouseDown input drag oldx oldy e _ =  do
  pagex <- J.getPageX e
  pagey <- J.getPageY e
  _ <- writeSTRef drag true
  _ <- writeSTRef oldx pagex
  _ <- writeSTRef oldy pagey
  r <- emptySTArray
  void $ pushSTArray r 1

mouseUp input drag e _ =  do
  _ <- writeSTRef drag false
  r <- emptySTArray
  void $ pushSTArray r 1


mouseMove input drag oldx oldy dx dy xangle yangle e _ =  void $ do
  drag_val <- readSTRef drag
  if drag_val == true 
    then do
      pagex <- J.getPageX e
      pagey <- J.getPageY e
      oldx_val <- readSTRef oldx
      oldy_val <- readSTRef oldy
      _ <- writeSTRef dx (modify pagex oldx_val 600.0)
      _ <- writeSTRef dy (modify pagey oldy_val 600.0)
      dx_val <- readSTRef dx 
      dy_val <- readSTRef dx 
      x_val <- readSTRef xangle 
      y_val <- readSTRef yangle 

      _ <- writeSTRef xangle (add x_val dx_val)
      _ <- writeSTRef yangle (add y_val dy_val)
      _ <- writeSTRef oldx pagex
      _ <- writeSTRef oldy pagey
      r <- emptySTArray
      void $ pushSTArray r 1
    else do
      r <- emptySTArray
      void $ pushSTArray r 1

rotateX angle x y z = do
  let rad = angle * pi / 180.0
  let cosa = cos rad
  let sina = sin rad
  let yy = y * cosa - z * sina
  let zz = y * sina + z * cosa
  [x,yy,zz] 


rotateY angle x y z = do
  let rad = angle * pi / 180.0
  let cosa = cos rad
  let sina = sin rad
  let xx = z * sina + x * cosa
  let zz = z * cosa - x * sina
  [xx,y,zz] 


project x y z = do
    let xRotQz = x*cos(qz)+y*sin(qz)
    let yRotQz = y*cos(qz)-x*sin(qz)
    let zRotQz = z
    let xRotQzQx = xRotQz
    let yRotQzQx = yRotQz*cos(qx)+zRotQz*sin(qx)
    let zRotQzQx = zRotQz*cos(qx)-yRotQz*sin(qx)
    let xRotQzQxQy = xRotQzQx*cos(qy)+zRotQzQx*sin(qy)
    let yRotQzQxQy = yRotQzQx
    [xRotQzQxQy, yRotQzQxQy] :: Array Number

multiply a b = a * b

add a b = a + b

fill v00 v01 v10 v11 v20 v21 v30 v31 ctx = do
  r <- emptySTArray
  _ <- fillPath ctx $ do
    _ <- moveTo ctx v00 v01
    _ <- lineTo ctx v10 v11
    _ <- lineTo ctx v20 v21
    _ <- lineTo ctx v30 v31
    _ <- closePath ctx
    void $ pushSTArray r 1
  void $ pushSTArray r 1


stroke v00 v01 v10 v11 v20 v21 v30 v31 ctx = do
  r <- emptySTArray
  _ <- strokePath ctx $ do
    _ <- moveTo ctx v00 v01
    _ <- lineTo ctx v10 v11
    _ <- lineTo ctx v20 v21
    _ <- lineTo ctx v30 v31
    _ <- closePath ctx
    void $ pushSTArray r 1
  void $ pushSTArray r 1



modify a b c = do
  ((a-b)*2.0*pi)/c

drawCube vertices faces ctx = do
    _ <- clearRect ctx { x: 0.0, y: 0.0, w: 650.0, h: 650.0 }
    verticesPixLoc <- emptySTArray
    void $ forE 0 8 $ \i ->  do
      xx <- peekSTArray vertices i 
      let yy = fromMaybe [] xx
      let zz = (yy !! 0)
      let aa = fromMaybe 0.0 zz
      let zz = (yy !! 1)
      let bb = fromMaybe 0.0 zz
      let zz = (yy !! 2)
      let cc = fromMaybe 0.0 zz
      let xyLoc = project aa bb cc :: Array Number
      let m_xy0 = xyLoc !! 0
      let m_xy1 = xyLoc !! 1
      let xy0 = fromMaybe 0.0 m_xy0
      let xy1 = fromMaybe 0.0 m_xy1
      let pix0 = xy0 + 650.0/2.0
      let pix1 = -1.0*xy1 + 400.0/2.0
      _ <- strokePath ctx $ arc ctx
        { x      : pix0
        , y      : pix1
        , r      : 4.0
        , start  : 0.0
        , end    : pi * 2.0
        }
      void $ pushSTArray verticesPixLoc [pix0,pix1]
    _ <- setFillStyle "rgb(255, 0, 0)" ctx
    _ <- setStrokeStyle "rgb(0, 0, 0)" ctx
    void $ forE 0 6 $ \i ->  do
      let m_i_face = faces !! i
      let i_face = fromMaybe [] m_i_face
      let mf0 = i_face !! 0
      let mf1 = i_face !! 1
      let mf2 = i_face !! 2
      let mf3 = i_face !! 3
      let f0 = fromMaybe 0 mf0
      let f1 = fromMaybe 0 mf1
      let f2 = fromMaybe 0 mf2
      let f3 = fromMaybe 0 mf3
      mv0 <- peekSTArray verticesPixLoc f0
      mv1 <- peekSTArray verticesPixLoc f1
      mv2 <- peekSTArray verticesPixLoc f2
      mv3 <- peekSTArray verticesPixLoc f3
      let v0 = fromMaybe [] mv0
      let v1 = fromMaybe [] mv1
      let v2 = fromMaybe [] mv2
      let v3 = fromMaybe [] mv3
      let mv00 = v0 !! 0
      let mv01 = v0 !! 1
      let mv10 = v1 !! 0
      let mv11 = v1 !! 1
      let mv20 = v2 !! 0
      let mv21 = v2 !! 1
      let mv30 = v3 !! 0
      let mv31 = v3 !! 1
      let v00 = fromMaybe 0.0 mv00
      let v01 = fromMaybe 0.0 mv01
      let v10 = fromMaybe 0.0 mv10
      let v11 = fromMaybe 0.0 mv11
      let v20 = fromMaybe 0.0 mv20
      let v21 = fromMaybe 0.0 mv21
      let v30 = fromMaybe 0.0 mv30
      let v31 = fromMaybe 0.0 mv31
      stroke v00 v01 v10 v11 v20 v21 v30 v31 ctx
      r <- emptySTArray
      void $ pushSTArray r 1
deceleration = 0.92


main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  body <- J.body

  canvas_jq <- J.getElementById "canvas"

  _ <- setFillStyle "rgb(39, 40, 34)" ctx
  drag <- newSTRef false
  old_x <- newSTRef 0.0
  old_y <- newSTRef 0.0
  dx <- newSTRef 0.0
  dy <- newSTRef 0.0
  dz <- newSTRef 0.0
  x_angle <- newSTRef 0.0
  y_angle <- newSTRef 0.0
  let sz = 100.0
  let msz = -100.0
  let vertices = [
        [msz, sz, msz],
        [sz, sz, msz], 
        [sz, msz, msz], 
        [msz, msz, msz], 
        [msz, sz, sz], 
        [sz, sz, sz], 
        [sz, msz, sz], 
        [msz, msz, sz]
    ]
  let faces = [[0,1,2,3],[1,5,6,2],[5,4,7,6],[4,0,3,7],[0,4,5,1],[3,2,6,7]]
  
  st_vertices <- emptySTArray

  void $ forE 0 8 $ \i ->  do
   let xx = (vertices !! i) 
   let yy = fromMaybe [] xx
   void $ pushSTArray st_vertices yy

  let updateCube = do
        t <- emptySTArray
        ddrag <- readSTRef drag
        dx_val <- readSTRef dx
        dy_val <- readSTRef dy
        if ddrag == false 
          then do
            _ <- writeSTRef dx (multiply dx_val deceleration)
            _ <- writeSTRef dy (multiply dy_val deceleration)
            dxx <- readSTRef dx
            dyy <- readSTRef dy
            x_a <- readSTRef x_angle
            y_a <- readSTRef y_angle
            _ <- writeSTRef x_angle (add x_a dxx)
            xangle <- readSTRef x_angle
            _ <- writeSTRef y_angle (add y_a dyy)
            r <- emptySTArray
            void $ pushSTArray r 1
          else do
            r <- emptySTArray
            void $ pushSTArray r 1
        void $ forE 0 8 $ \i ->  do
          mvi <- peekSTArray st_vertices i
          let vi = fromMaybe [] mvi
          let mx = vi !! 0 
          let my = vi !! 1 
          let mz = vi !! 2
          let x = fromMaybe 0.0 mx 
          let y = fromMaybe 0.0 my 
          let z = fromMaybe 0.0 mz 
          yangle <- readSTRef y_angle
          xangle <- readSTRef x_angle
          let v = rotateX (yangle*20.0) x y z
          let mvx = v !! 0
          let mvy = v !! 1
          let mvz = v !! 2
          let vx = fromMaybe 0.0 mvx
          let vy = fromMaybe 0.0 mvy
          let vz = fromMaybe 0.0 mvz
          let vv = rotateY (20.0*xangle) vx vy vz
          void $ pushSTArray t vv
          r <- emptySTArray
          void $ pushSTArray r 1
        drawCube t faces ctx
        requestAnimationFrame updateCube

  drawCube st_vertices faces ctx 
  J.on "mousedown" (mouseDown canvas drag old_x old_y) canvas_jq
  J.on "mouseup" (mouseUp canvas drag) body
  J.on "mousemove" (mouseMove canvas drag old_x old_y dx dy x_angle y_angle) canvas_jq
  J.on "mouseout" (mouseMove canvas drag old_x old_y dx dy x_angle y_angle) canvas_jq
  updateCube
