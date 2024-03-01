{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (unless, when)
import Graphics.Rendering.OpenGL as GL 
import Graphics.Rendering.OpenGL.GLU.Matrix as GM
       
-- http://hackage.haskell.org/package/FTGL-2.1/docs/Graphics-Rendering-FTGL.html
-- KEY: render font "Hello"
import qualified Graphics.Rendering.FTGL as FT
       
import qualified Graphics.UI.GLFW as G
import qualified  Graphics.UI.GLUT as GLUT

import Bindings.GLFW 

import System.Exit
import System.IO
import Control.Monad
import System.Random
import Data.Set(Set) 
import Data.IORef 
import Data.Maybe
import qualified Data.Set as S 
import Linear.V3
import Linear.V3(cross) 
import Linear.Vector
import Linear.Matrix
import Linear.Projection as P
import Linear.Metric(norm, signorm)
import Text.Regex
import System.FilePath ((</>))
       
import AronModule
import AronGraphic
import AronDevLib
import AronOpenGL

-- beg screenbuffer to image .png file
import Data.Word
import Foreign.Ptr
import qualified Data.Vector.Storable as STO
import Foreign.ForeignPtr
import Codec.Picture.Types -- from the JuicyPixels librar
import Codec.Picture.Png


import System.CPUTime

-- tiny utility functions, in the same spirit as 'maybe' or 'either'
-- makes the code a wee bit easier to read
-- compile: ghc -o glfw glfw.hs

--t1 (a, b, c) = a
--t2 (a, b, c) = b 
--t3 (a, b, c) = c 

winWidth  = 1000::Int
winHeight = 1000::Int
                
type VeG = Vector3 GLdouble
         
c0 = Vertex3 0   0 0 
x0 = Vertex3 0.5   0 0 
y0 = Vertex3 0   0.5 0 
z0 = Vertex3 0     0 0.5 
xyz = Vertex3 0.5 0.5 0.5 


toVertex3::[GLfloat] -> Vertex3 GLfloat 
toVertex3 [a, b, c] = Vertex3 a b c
toVertex3 _         = Vertex3 0 0 0



vertex3ToList::Vertex3 a -> [a]
vertex3ToList (Vertex3 a b c) = [a, b, c]


addx::(Num n)=> n -> (n, n, n) -> (n, n, n)
addx m (a, b, c) = (m + a, b, c)

addy::(Num n)=> n -> (n, n, n) -> (n, n, n)
addy m (a, b, c) = (a, m + b, c)

mx::(Num n)=> n -> (n, n, n) -> (n, n, n)
mx m (a, b, c) = (m, b, c)

my::(Num n)=> n -> (n, n, n) -> (n, n, n)
my m (a, b, c) = (a, m, c)

-- | mod for Double, 6.28 `fmod` 2 => 0.28
fmod::Double->Integer->Double
fmod  a n = a - du
            where     
                num = realToFrac (div (round a) n)
                du = realToFrac ((round num)*n)

vertexList = [
            (0.3, 0.4, 0),
            (0.6, 0.9, 0),
            (0.5, 0.2, 0),
            (0.8, 0.3, 0),
            (0.4, 0.6, 0),
            (0.8, 0.1, 0),
            (0.6, 0.3, 0),
            (0.9, 0.5, 0),
            (0.9, 0.4, 0)
            ]

--vertexList2 = [
--            Vertex3 0.9 0.8 0,
--            Vertex3 0.5 0.1 0,
--
--            Vertex3 0.7 0.1 0,
--            Vertex3 0.2 0.4 0,
--            Vertex3 0.9 0.7 0,
--
--            Vertex3 0.71 0.1 0,
--            Vertex3 0.2 0.9 0,
--            Vertex3 0.23 0.3 0,
--            
--            Vertex3 0.5 0.5 0,
--            Vertex3 0.1 0.9 0,
--            Vertex3 0.2 0.31 0,
--            
--            Vertex3 0.471 0.21 0,
--            Vertex3 0.442 0.34 0,
--            Vertex3 0.2333 0.6 0,
--            
--            Vertex3 0.555 0.245 0,
--            Vertex3 0.111 0.399 0,
--            Vertex3 0.222 0.231 0,
--
--
--            Vertex3 0.89 0.33 0,
--            Vertex3 0.21 0.31 0,
--            Vertex3 0.69 0.13 0,
--            Vertex3 0.121 0.51 0,
--            Vertex3 0.49 0.43 0,
--            Vertex3 0.44 0.66 0
--            ]
--
vertexList2 = [
--    Vertex3 0.38 0.41 0.44,
--    Vertex3 0.72 6.0e-2 0.23,
--    Vertex3 0.73 0.21 0.38,
--    Vertex3 0.74 0.34 7.0e-2
        Vertex3 0.16 0.43 0,
        Vertex3 0.18 0.16 0,
        Vertex3 0.28 0.85 0,
        Vertex3 0.95 0.82 0
    ]
--vertexList2 = [
--            Vertex3 0.9 0.8 0,
--            Vertex3 0.23 0.4 0,
--            Vertex3 0.2 0.31 0,
--            Vertex3 0.1 0.95 0
----            Vertex3 0.89 0.33 0,
----            Vertex3 0.44 0.66 0
--            ]
--

-- compute the insection of two line
-- compute the inverse of 2x2 matrix
-- solve [s, t]
--insertLine::Vertex3 a ->Vertex3 a ->Vertex3 a ->Vertex3 a -> Vertex3 a
--insertLine (Vertex3 x0 y0 z0) (Vertex3 x1 y1 z1) (Vertex3 a0 b0 c0)(Vertex3 a1 b1 c1) =



mergeChunk::Int->[(GLfloat, GLfloat, GLfloat)]->[(GLfloat, GLfloat, GLfloat)]
mergeChunk n c = mergeList  (take n c)  (take n $ drop n c) 

nStep::Float
nStep = 40

sphere::[(GLfloat, GLfloat, GLfloat)]
sphere = [(cos(del*k)*cos(del*i), 
          _sin(del*k), 
          cos(rf del*k)*sin(rf del*i)) | k <- [1..n], i <-[1..n]]
            where 
                del = rf(2*pi/(n-1))
                n = nStep 

shear f = do
   m <-  (newMatrix RowMajor [1,f,0,0
                             ,0,1,0,0
                             ,0,0,1,0
                             ,0,0,0,1])
   multMatrix (m:: GLmatrix GLfloat)

poly::[Double]->[Double]->[Double]
poly [] _ = [] 
poly _ [] = [] 
poly xs sx = map(\s -> sum s) $ map(\x -> map(\(c, p) -> c*(x^p)) po) sx
            where
                po = zip xs [0..]

-- | Torus paremater equation center at (0, 0, 0)
-- | Torus equation: http://xfido.com/html/indexThebeautyofTorus.html 
torus::[(GLfloat, GLfloat, GLfloat)]
torus= [((br + r*cos(del*i))*cos(del*k), 
        sin(rf del*i), 
        (br + r*cos(rf del*i))*sin(rf del*k) ) | i <- [1..n], k <-[1..n]]
        where 
            del = rf(2*pi/(n-1))
            n = nStep 
            r = 0.2
            br = 0.3

slop::(GLfloat, GLfloat, GLfloat) -> (GLfloat, GLfloat, GLfloat) -> Double
slop (x1, y1, z1) (x2, y2, z2) = rf(y2 - y1) / rf(x2 - x1)

slop'::Vertex3 GLfloat -> Vertex3 GLfloat -> Float 
slop' (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2)  = rf(y2 - y1) / rf(x2 - x1) 

cosVec::Vertex3 GLfloat -> Vertex3 GLfloat -> Float
cosVec (Vertex3 x0 y0 z0) (Vertex3 x1 y1 z1) = d/n 
                    where
                        vx = Vertex3 (-1) 0 0  -- norm lv = 1
                        v01= Vertex3 (x1 - x0) (y1 - y0) (z1 - z0)
                        -- TODO: should use Vector3?
                        dot (Vertex3 a0 b0 c0) (Vertex3 a1 b1 c1) = a0*a1 + b0*b1 + c0*c1 
                        d = dot vx v01 
                        norm v = sqrt $ dot v v 
                        -- n = if norm v01 > 0 then norm v01 else 1
                        n = norm v01

cosVec3::Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> Float
-- cosVec3 (Vertex3 x0 y0 z0) (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = d/((norm v10)*(norm v12))
cosVec3 (Vertex3 x0 y0 z0) (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = d/(n1*n2)
                    where
                        -- vector 1 -> 0 
                        v10 = Vertex3 (x0 - x1) (y0 - y1) (z0 - z1)
                        -- vector 1 -> 2 
                        v12 = Vertex3 (x2 - x1) (y2 - y1) (z2 - z1)
                        -- dot v10 v12
                        dot (Vertex3 a0 b0 c0) (Vertex3 a1 b1 c1) = a0*a1 + b0*b1 + c0*c1 
                        d = dot v10 v12
                        norm v = sqrt $ dot v v
                        n1 = norm v10
                        n2 = norm v12
--                        n1 = if norm v10 /= 0 then norm v10 else 1
--                        n2 = if norm v12 /= 0 then norm v12 else 1

det::Vertex3 GLfloat -> Vertex3 GLfloat -> Vertex3 GLfloat -> Float
det (Vertex3 x0 y0 z0) (Vertex3 x1 y1 z1) (Vertex3 x2 y2 z2) = det' v10 v12
                    where
                        -- vector 1 -> 0 
                        v10 = Vertex3 (x0 - x1) (y0 - y1) (z0 - z1)
                        -- vector 1 -> 2 
                        v12 = Vertex3 (x2 - x1) (y2 - y1) (z2 - z1)
                        -- dot v10 v12
                        --  1 0 
                        --  0 1 
                        -- | => det > 0, left turn from +X-axis to +Y-axis (CCW)
                        det' (Vertex3 x0 y0 _) (Vertex3 x1 y1 _) = x0*y1 - y1*y0 


tryDrawCircle::IO()
tryDrawCircle = do
    preservingMatrix $ do
        forM_ list $ \x -> do
            rotate (x)$ (Vector3 1 1 1 :: VeG)
            drawCircle' vx  0.04 
            where
                list = [ x | x <- let d = 360/100 in map (*d) [1..100]]
                vx = Vertex3 0.5 0.5 0


torus2::[(GLfloat, GLfloat, GLfloat)]
torus2= [((br + r*_cos(del*i))*_cos(del*k) + _cos(del*i), 
        _sin(del*i)*_cos(del*k), 
        (br + r*_cos(del*i))*_sin(del*k) ) | i <- [1..n], k <-[1..n]]
        where 
            del = 2*pi/(n-1)
            n = nStep 
            r = 0.1
            br = 0.2



--circle::[(GLfloat, GLfloat, GLfloat)]
--circle =[ let alpha = (pi2*n)/num in (r*_sin(alpha), r*_cos(alpha), 0) | n <- [1..num]]
--        where
--            num = 40 
--            r = 0.2 
--            pi2 = 2*pi::Float 


curve1::[(GLfloat, GLfloat, GLfloat)]
curve1 = [let x = d*k  in 
            (x, f x, 0) | k <- [a..b]] 
            where
                a = -300
                b = 300
                d = 0.01
                f x = x*x*x

pointList::[(GLfloat, GLfloat, GLfloat)]
pointList = [let x = d*k  in 
            (x, f x, 0) | k <- [a..b]] 
            where
                a = -300
                b = 300
                d = 0.01
                f x = x*x*x

drawDots::[Vertex3 GLfloat] -> IO ()
drawDots cx = mapM_ drawDot cx

data Seg a = Smk (a, a) deriving (Show)

beg::(Num a)=>Seg a -> a
beg (Smk (p0, p1)) = p0

end::(Num a)=>Seg a -> a
end (Smk (p0, p1)) = p1 

contains::Vertex3 a -> Seg (Vertex3 a) -> Bool
contains _ _ = undefined

isCol::Vertex3 a -> Seg (Vertex3 a) -> Bool
isCol _ _ = undefined

dst::Seg (Vertex3 a) -> a 
dst _ = undefined

getParameter::Vertex3 a -> Seg (Vertex3 a) -> (a, a) 
getParameter _ _ = undefined

isIntersected::Seg (Vertex3 a) -> Seg(Vertex3 a) -> Bool
isIntersected _ _ = undefined

intersect::Seg (Vertex3 a) -> Seg(Vertex3 a) -> Maybe (Vertex3 a)
intersect _ _ = undefined

extendSeg::Seg (Vertex3 a) -> a -> (Vertex3 a)
extendSeg _ _ = undefined


randomVertex3::Int-> IO [Vertex3 GLfloat]
randomVertex3 n = randomDouble n >>= \x -> return $ map(\w ->Vertex3 (head w) ((head . tail) w) (last w)) $ case (mod n 3) of 
                                                                            0 -> init $ partList 3 $ map (realToFrac) x
                                                                            _ -> partList 3 $ map (realToFrac) x

vx::Vertex3 GLdouble -> GLdouble
vx (Vertex3 x _ _) = x
                     
vy::Vertex3 GLdouble -> GLdouble
vy (Vertex3 _ y _) = y
                     
vz::Vertex3 GLdouble -> GLdouble
vz (Vertex3 _ _ z) = z
                     
-- | line segements from points with LineStrip
-- | e.g. draw curve, line
renderCurve::[Vertex3 GLfloat]->IO()
renderCurve points = do 
    renderPrimitive LineStrip $ mapM_(\vx -> vertex $ vx) points


---- | Given a function f, interval (a, b), 
----   draw the curve from a to b
--drawCurve::(GLfloat -> GLfloat) -> (GLfloat, GLfloat) ->Color3 GLdouble  -> IO()
--drawCurve f (a, b) c = renderPrimitive LineStrip $ mapM_(\vx -> do 
--                                            color c 
--                                            vertex $ vx) $ curvePt f (a, b) 

toStr::(Show a)=>[a]->[String]
toStr [] = [] 
toStr xs = map(\x -> show x) xs 

mybool :: Bool -> a -> a -> a
mybool b falseRes trueRes = if b then trueRes else falseRes

myUnless :: Monad m => m Bool -> m () -> m ()
myUnless action falseAction = do
    b <- action
    unless b falseAction

myMaybe :: Maybe a -> b -> (a -> b) -> b
myMaybe m nothingRes f = case m of
    Nothing -> nothingRes
    Just x  -> f x
    
-- type ErrorCallback = Error -> String -> IO ()
myErrorCallback :: G.ErrorCallback
myErrorCallback err description = hPutStrLn stderr description


simpleKeyBoardCallBack :: IORef (Set G.Key) -> G.KeyCallback
simpleKeyBoardCallBack ref window key scanCode keyState modKeys = do
    putStrLn $ "simpleKeyBoardCallBack=>keyState" ++ show keyState ++ " " ++ "simpleKeyBoardCallBack=>key=" ++ show key
    case keyState of
        G.KeyState'Pressed -> modifyIORef ref (S.insert key) >> readIORef ref >>= \x -> print $ "inside simpleKeyBoardCallBack=> readIORef ref=>" ++ show x 
        G.KeyState'Released -> modifyIORef ref (S.delete key)
        _ -> return ()
    when (key == G.Key'Escape && keyState == G.KeyState'Pressed)
        (G.setWindowShouldClose window True)


readFileTo2dList::FilePath->String->IO[[String]]
readFileTo2dList fp s = do
        -- ls <- readFileToList fp 
        ls <- readFileLatin1ToList fp 
        let ls' = map(\x -> filter(\c -> (length.trim) x > 0) $ splitRegex (mkRegex s) x) $ filter(\x -> length x > 0) $ map(trim) ls 
        return ls'



{-|
    KEY: save image, save png, opengl save image, save png opengl

    https://www.reddit.com/r/haskell/comments/dee0iz/converting_opengl_window_to_a_png_image_in_haskell/
    read pixels, convert them to first a Vector then a JuicyPixel image
    and save the image as a PNG file

    readPixels::(Position x y) -> (Size w h) -> PixelData a -> IO ()
    readPixels (Position x y) (Size w h) pd =
      withPixelData pd $ glReadPixels x y w h


    data PixelData a = PixelData PixelFormat DataType (Ptr a)

    https://www.khronos.org/registry/OpenGL-Refpages/gl4/html/glReadPixels.xhtml

    glReadPixels(GLint x       ,
                 GLint y       ,
                 GLsizei width ,
                 GLsizei height,
                 GLenum  format,
                 GLenum  type  ,
                 void * data);

-}
saveImage :: G.Window -> FilePath -> IO ()
saveImage window fp = do
  -- (Int, Int) <- G.getFramebufferSize window
  (w, h) <- G.getFramebufferSize window
  let w' = fromIntegral w
  let h' = fromIntegral h
  -- currentWindow $= Just window
  -- Size w h <- get windowSize
  let npixels = fromIntegral (w*h) :: Int
      nbytes  = 3*npixels
  fptr <- mallocForeignPtrArray nbytes :: IO (ForeignPtr Word8)
  -- withForeignPtr::ForeignPtr a -> (Ptr a -> IO b) -> IO b
  withForeignPtr fptr $ \ptr -> do
    let pdata = GL.PixelData GL.RGB GL.UnsignedByte ptr :: PixelData Word8
    readPixels (Position 0 0) (Size w' h') pdata
    -- readPixels (Position 0 0) (Size Int32 Int32) pdata
  let fptr' = castForeignPtr fptr :: ForeignPtr (PixelBaseComponent PixelRGB8)
  let imgdata = STO.unsafeFromForeignPtr0 fptr' npixels :: STO.Vector (PixelBaseComponent PixelRGB8)
  let image = Image (fromIntegral w) (fromIntegral h) imgdata :: Image PixelRGB8
  writePng fp image
        
saveImageCountName::G.Window -> FilePath -> IORef Integer -> IO()
saveImageCountName window dir ref = do
  count <- readIORef ref
  let fp = dir </> "img" ++ (show count) ++ ".png"
  saveImage window fp
  modifyIORef ref (+1)

mymain :: IO ()
mymain = do
  G.setErrorCallback (Just myErrorCallback)
  -- finally add double buffer
  successfulInit <- G.init
  -- G.windowHint (G.WindowHint'DoubleBuffer True)

  
  mapM_ G.windowHint
    [ G.WindowHint'Samples (Just 4) -- 4x antialiasing
    , G.WindowHint'DoubleBuffer True
      
    -- There is error if following is used
    -- , G.WindowHint'ContextVersionMajor 3 -- OpenGL 3.3
    -- , G.WindowHint'ContextVersionMinor 3
    -- we don't want the old OpenGL
    -- , G.WindowHint'OpenGLProfile G.OpenGLProfile'Core
    ]
  
  -- if init failed, we exit the program
  mybool successfulInit exitFailure $ do
      mw <- G.createWindow winWidth winHeight "Simple example, haskell style" Nothing Nothing
      printGLVersion $ fromJust mw

      myMaybe mw (G.terminate >> exitFailure) $ \window -> do
          G.makeContextCurrent mw
          refFrame <- (timeNowMilli >>= \x -> newIORef FrameCount{frameTime = x, frameCount = 1})
          refRef <- newIORef 100
          mainLoop window refFrame refRef
          G.destroyWindow window
          G.terminate
          exitSuccess





{-|
    === KEY: update FrameCount, 100 <= counter <= 200

    output: 1 <= count <= 100
-}
updateRefFrame::Integer -> IORef FrameCount -> IO (Integer, GLdouble)
updateRefFrame s currRef = do
          cref <- readIORef currRef
          -- frameCount currRef <= 200
          writeIORef currRef FrameCount{frameTime = s, frameCount = let c = frameCount cref in c < 200 ? c + 1 $ c}
          currRef' <- readIORef currRef
          let count = (frameCount currRef') - 100
          let delta = (fi count) * 0.005 :: GLdouble
          return (count, delta)

{-|
    === KEY: move along x Axis
-}
moveX::Integer -> IORef FrameCount -> IO()
moveX s currRef = do
  (count, delta) <- updateRefFrame s currRef
  translate (Vector3  delta 0 0 :: VeG)
    
moveY::Integer -> IORef FrameCount -> IO()
moveY s currRef = do
  (count, delta) <- updateRefFrame s currRef
  translate (Vector3 0 delta 0 :: VeG)

moveZ::Integer -> IORef FrameCount -> IO()
moveZ s currRef = do
  (count, delta) <- updateRefFrame s currRef
  translate (Vector3 0 0 delta :: VeG)

moveTo::GLdouble -> GLdouble -> GLdouble -> IO()
moveTo x y z = do
  translate (Vector3  x y z :: VeG)

moveTo2::(GLdouble, GLdouble, GLdouble) -> IO()
moveTo2 (x, y, z) = do
  translate (Vector3 x y z :: VeG)

mybutton::G.Window -> IO()
mybutton w = do
  let x = 0.2::GLdouble
  let y = 0.1::GLdouble
  let dclen = 1 - (-1)  -- device coordinates   x: [-1, 1]  y: [-1, 1]
  let pixelX = (fi winWidth / fi dclen) * (rf x)  -- (1000 / 2) * 0.1
  let pixelY = (fi winWidth / fi dclen) * (rf y)  -- (1000 / 2) * 0.1
  let cenPixelX = fi winWidth/2  :: GLdouble
  let cenPixelY = fi winHeight/2 :: GLdouble
  pressRel <- G.getMouseButton w G.MouseButton'1
  case pressRel of
    G.MouseButtonState'Pressed -> do
      pos <- G.getCursorPos w
      let screenX   = fst pos/fi winWidth
      let absX = abs $ cenPixelX - (fst pos)
      let absY = abs $ cenPixelY - (snd pos)
      print $ (take 10 $ repeat '-') ++ "G.MouseButtonState'Pressed" ++ (take 10 $ repeat '-')
      print $ "absX=" ++ (show absX)
      print $ "absY=" ++ (show absY)

      {-|
          ->leftX               rightX<-
                /--------------/         upY 
               /              /
              /    (0 0)     /
             /              /
            /--------------/             downY
      -}
      let leftX   = cenPixelX - rf pixelX
      let rightX  = cenPixelX + rf pixelX
      let upY     = cenPixelY - rf pixelY
      let downY   = cenPixelY + rf pixelY
      print $ "position      =" ++ (show pos)
      print $ "pixel: leftX  =" ++ (show leftX)
      print $ "pixel: rightX =" ++ (show rightX)
      print $ "pixel: upY    =" ++ (show upY)
      print $ "pixel: downY  =" ++ (show downY)

      let cursorX = fst pos
      let cursorY = snd pos
      if leftX <= cursorX && cursorX <= rightX && upY <= cursorY && cursorY <= downY
        then do
          GM.lookAt (Vertex3 0.2 0.2 0.2::Vertex3 GLdouble) (Vertex3 0 0 0:: Vertex3 GLdouble) (Vector3 0 1 0 :: VeG)
          drawRectColor2 white (Vertex3 x y 0.0, Vertex3 (-x) (-y) 0.0)
          drawSegNoEnd gray (-x, y,  0) (x,  y, 0)  -- top horizontal line
          drawSegNoEnd gray (-x, -y, 0) (-x, y, 0)  -- left vertical line
        else do
          print "Cursor is outside the button"
          drawRectColor2 white (Vertex3 x y 0.0, Vertex3 (-x) (-y) 0.0)
      fl
    _                          -> do
      drawRectColor2 white (Vertex3 x y 0.0, Vertex3 (-x) (-y) 0.0)
  -- drawRectColor2 white (Vertex3 x y 0.0, Vertex3 (-x) (-y) 0.0)
  return ()


{-|
    === KEY: xy-plane button

    (0.2 0.2)  (0.4 0.4)
-}
mybutton2::G.Window -> Vertex3 GLdouble -> Vertex3 GLdouble -> IO()
mybutton2 w p0@(Vertex3 x0 y0 z0) p1@(Vertex3 x1 y1 z1) = do
  preservingMatrix $ do
    let p@(x, y, z) = ((x0 + x1)/2.0, (y0 + y1)/2.0, (z0 + z1)/2.0)
    translate $ Vector3 x y z
    let xx = abs (x0 - x)/2.0
    let yy = abs (y0 - y)/2.0
    drawRectColor2 green (Vertex3 (-xx) (-yy) 0, Vertex3 xx yy 0)
  
  
    
{-|
    @

    moveFromTo(ptFrom, ptTo) -> speed -> step_counter -> IORef FrameCount -> IO()

    linear

    @
-}
moveFromTo::(Vertex3 GLdouble, Vertex3 GLdouble) -> Integer -> Integer -> IORef FrameCount -> IO()
moveFromTo (p1, p2) speed count refFrameCount = do
  -- let m = 100::Integer
  print $ "count=" ++ (show count)
  let v = (p2 -: p1)   -- p1 -> p2
  let v' = (/:) v speed
  let vp = p1 -: p0
  translate vp
  if nr2 (fi count *: v') <= nr2 v then translate (fi count *: v') else translate v
  -- translate (fi n *: v')
  where
   p0 = Vertex3 0.0 0.0 0.0 :: Vertex3 GLdouble




{-|
moveFromTo3::(GLdouble -> GLdouble, GLdouble -> GLdouble, (GLdouble, GLdouble)) -> Integer -> Integer -> IO() -> IO()
moveFromTo3 (f, h, (x₀, x₁)) speed count draw = do
  let delta = (x₁ - x₀)/100.0
  let tt = map (\x -> x₀ + x * delta) [0..100]
  let zz = map (\t -> (f t, g t)) tt
  print $ "count=" ++ (show count)
  let lx = map (\x -> rf x) $ map (\x -> x0 + x * delta) [0..100]
  let ls = map (\x -> Vector3 x (f x) 0.0::VeG) lx
  let inx = speed * count - 1 in (inx <= 100) ? (translate $ ls ! fi inx) $ (translate $ ls ! 99)
  draw
  where
    (!) = (!!)
-}



    
{-|
    === KEY: animate counter

  @
          refFrame <- (timeNowMilli >>= \x -> newIORef FrameCount{frameTime = x, frameCount = 1})

          (count, isNext, currRef) <- readRefFrame refFrame

          -- count   => start from 1 to 100, 1 <= count <= 100 and stop at 100
          -- isNext  => Bool, the delay is <= 20::Integer then True else False
          -- currRef => old Ref or updated Ref dependen on whether isNext is True or False

          when isNext $ do
            draw1
            draw2
  @
-}
{-|
data FrameCount = FrameCount{frameTime::Integer, frameCount::Integer}
                
readRefFrame::IORef FrameCount -> IO(Integer, Bool, FrameCount)
readRefFrame refFrame = do
  let timeDiff = 20::Integer
  currTime <- timeNowMilli
  oldFrame <- readIORef refFrame
  let oldTime = frameTime oldFrame
  let oldCount = frameCount oldFrame
  let isNext = currTime - oldTime > timeDiff ? True $ False
  if isNext then do
    let newCount = let c = oldCount in c < 100 ? c + 1 $ c
    writeIORef refFrame FrameCount{frameTime = currTime, frameCount = newCount}
    newFrame <- readIORef refFrame
    return (newCount, isNext, newFrame)
  else do
    return (oldCount, isNext, oldFrame)
-}
   
mainLoop :: G.Window -> IORef FrameCount -> IORef Integer -> IO ()
mainLoop w refFrame intRef = myUnless (G.windowShouldClose w) $ do
    -- writeToFile "/tmp/g1.x" $ toStr torus  
    lastFrame <- maybe 0 realToFrac <$> G.getTime

    -- G => GLFW function
    (width, height) <- G.getFramebufferSize w
    let ratio = fromIntegral width / fromIntegral height
    viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
    GL.clear [ColorBuffer, DepthBuffer]
    -- lighting info
    diffuse  (Light 0) $= lightDiffuse
    ambient  (Light 0) $= lightAmbient
    specular (Light 0) $= lightSpecular
    position (Light 0) $= lightPosition

    -- GLUT.keyboardMouseCallback $= Just keyboardMouse
    -- light    (Light 0) $= Enabled
    -- disable to show color
    -- lighting           $= Enabled
    depthFunc      $= Just Lequal
    blend          $= Enabled
    lineSmooth     $= Enabled
    -- end lighting info
    matrixMode $= Projection
    loadIdentity
    let fovy = 60.0; aspect = 1.0; zNear = 2.0; zFar = (-2)
          in GM.perspective fovy aspect zNear zFar 
    -- GLUT.ortho (negate ratio) ratio (negate 1.0) 1.0 1.0 (negate 1.0)
    -- GL.ortho 0 (fi width) (fi height) 0 (-1) (1::Double)

    matrixMode $= Modelview 0
    loadIdentity
    -- lookAt(Vertex3 pos of camera, Vertex3 point at, Vector up)
    -- GM.lookAt (Vertex3 0.1 0.1 0.5::Vertex3 GLdouble) (Vertex3 0 0 0:: Vertex3 GLdouble) (Vector3 0 1 0 :: VeG)
    -- lookAt from center up

    -- 
    -- http:// localhost/image/opengl_3dlook.png
    -- 
    -- look at origin from Vertex3 0.5 0.5 0.5
    -- GM.lookAt (Vertex3 0.5 0.5 0.5::Vertex3 GLdouble) (Vertex3 0 0 0:: Vertex3 GLdouble) (Vector3 0 1 0 :: VeG)

    -- 
    -- http://l_₋——–===—__—–====__₋— ocalhost/image/opengl_xyplane.png
    -- 
    -- standard xy-plane

    lv <- readGLScript "/tmp/draw.x"
    let lx = takeVertex3 lv
    if len lv > 0 then do
      let ver = head lx
      GM.lookAt ver (Vertex3 0 0 0:: Vertex3 GLdouble) (Vector3 0 1 0 :: VeG)
    else return ()
      
    ref <- newIORef S.empty
    -- G => GLFW function
    -- ESC to exit
    G.setKeyCallback w (Just $ simpleKeyBoardCallBack ref)
    
    {-|
    pressRel <- G.getMouseButton w G.MouseButton'1
    case pressRel of
      G.MouseButtonState'Pressed -> do
        pos <- G.getCursorPos w
        print pos
      _                          -> return ()
    -}
    
    -- mouse event
    -- render sphere

    renderCoordinates
    let ptls = [(0.3, 0.3, 0.0::GLfloat), (0.4, 0.4, 0.0::GLfloat), (0.4, 0.6, 0.0::GLfloat)]
    -- drawPrimitive LineLoop (Color3 0.5 0.5 0.5) ptls
    -- drawSegment red (x0, y0)

    -- start <- readIORef refFrame
    -- diff <- getCPUTime >>= \x -> return $ fromIntegral (x - start)
    -- diff <- (-) <$> getCPUTime <*> readIORef refFrame
    (count, isNext, currRef) <- readRefFrame refFrame
        
    -- s <- timeNowMilli
    -- currRef <- readIORef refFrame
    -- let t = frameTime currRef
    -- (count, delta) <- updateRefFrame curr refFrame

    
    when isNext $ do -- 1000/3 = 300
      -- let fcount = frameCount currRef
      -- let fp = "image/img" ++ (show fcount) ++ ".png"
      -- saveImage w fp
      -- writeIORef refFrame FrameCount{frameTime = s, frameCount = (frameCount currRef + 1)}
      {-|
      preservingMatrix $ do
        currRef <- readIORef refFrame
        let fcount = frameCount currRef
        let fp = "image/img" ++ (show fcount) ++ ".png"
        writeIORef refFrame FrameCount{frameTime = s, frameCount = (frameCount currRef + 1)}
        let m = (frameCount currRef) - 100
        let u = (fromIntegral m) * 0.02 :: GLdouble
        -- translate (Vector3  0 0 u :: VeG)
        -- drawRect (Vertex3 (-0.2) 0.2 0.0::Vertex3 GLfloat, Vertex3 0.2 (-0.2) 0.0::Vertex3 GLfloat)
        -- drawRect2d (0.2, 0.3)
        -- drawRectFill2d green (0.3, 0.2)
        -- drawCircleXYZColor (Vertex3 0 0 0 ::Vertex3 GLfloat) 0.4 green
        let x = fun44 1
        -- drawSphere
        -- drawCylinder
        
        -- saveImage w fp
        return ()
      -}
      preservingMatrix $ do
        -- (count, delta) <- updateRefFrame s refFrame
        -- currRef <- readIORef refFrame
        -- let fcount = frameCount currRef
        -- let fp = "image/img" ++ (show fcount) ++ ".png"
        let fp = "image/img" ++ (show count) ++ ".png"
        -- print fp
        -- writeIORef refFrame FrameCount{frameTime = s, frameCount = (frameCount currRef + 1)}
        -- let m = (frameCount currRef) - 100
        -- let u = (fromIntegral m) * 0.005 :: GLdouble
        -- translate (Vector3  delta 0 0 :: VeG)
        -- moveY count refFrame
        -- drawPrimitive LineLoop (Color3 0.1 0.4 0.8) ptls
        -- drawCircleVec 0.2 (Vertex3 0.0 0.0 0.0) (Vector3 0.2 0 0)
        -- drawGrid
        -- drawConic

    
        -- moveX count refFrame
        -- drawConic
        let p0 = Vertex3 0.0 0.0 0.0 :: Vertex3 GLdouble
        let p1 = Vertex3 0.0 0.4 0.0 :: Vertex3 GLdouble
        drawSegmentFromTo2 blue [p0, p1]
        let speed = 1::Integer
        -- moveFromTo (p0, p1) speed count refFrame
        let f = \x -> -(x^2)
        let iv = (-1, 1)
        let g = \x -> x
        drawCurveD f iv green

        -- mybutton w
        
        moveFromTo2 (f, iv) speed count $ drawRect2d (0.1, 0.1) >> drawRect2d (0.2, 0.2)
    
        -- drawRect2d (0.1, 0.1)
    
        -- drawCurve f (negate 1.0, 1.0) green
        -- drawConic
        -- drawQuadsColor red [Vertex3 0.0 0.0 0.0, Vertex3 0.2 0.0 0.0, Vertex3 0.0 0.2 0.0, Vertex3 0.0 0.0 0.2]
        -- saveImage w fp
        -- count33 <- readIORef intRef
        -- pp count33
        -- mybutton w



        {-|
        renderPrimitive Lines $ mapM_(\v3 -> do
                 color green
                 vertex v3) [Vertex3 0 0 0 :: Vertex3 GLdouble, Vertex3 0.5 0 0 :: Vertex3 GLdouble]


        drawSegment gray let
                            p0 = Vertex3 0   0.02   0::Vertex3 GLfloat
                            p1 = Vertex3 0.1 0.02   0::Vertex3 GLfloat
                          in (p0, p1)
    
        drawSegment red   let
                            p0 = Vertex3 0   0.04   0::Vertex3 GLfloat
                            p1 = Vertex3 0.5 0.04   0::Vertex3 GLfloat
                          in (p0, p1)
    
        drawSegment white let
                            p0 = Vertex3 0   0.06   0::Vertex3 GLfloat
                            p1 = Vertex3 1   0.06   0::Vertex3 GLfloat
                          in (p0, p1)
        -}
        -- saveImageCountName w "image" intRef
        
    G.swapBuffers w
    G.pollEvents
    mainLoop w refFrame intRef
  where
    (!) = (!!)
    
main = mymain

