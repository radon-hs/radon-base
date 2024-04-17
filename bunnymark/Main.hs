{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- Writing performant h-raylib code requires the use of pointers and other
-- un-Haskelly functionality. Unfortunately, this cannot be avoided.

module Main where

import Control.Monad (forM_, unless, void, when)
import Foreign
  ( Ptr,
    free,
  )
import Foreign.C (withCString)
import Radon.Access.Util (access)
import Raylib.Core
  ( beginDrawing,
    c'getMouseX,
    c'getMouseY,
    c'getRandomValue,
    changeDirectory,
    clearBackground,
    endDrawing,
    getApplicationDirectory,
    getFrameTime,
    getScreenHeight,
    getScreenWidth,
    initWindow,
    isMouseButtonDown,
    setTargetFPS,
    windowShouldClose, isKeyPressed,
  )
import Raylib.Core.Shapes (drawRectangle)
import Raylib.Core.Text (drawFPS, drawText)
import Raylib.Core.Textures (c'drawTexture, c'loadTexture, c'unloadTexture)
import Raylib.Types (Color (Color), MouseButton (MouseButtonLeft), KeyboardKey (KeyC))
import Raylib.Util (inGHCi, raylibApplication)
import Raylib.Util.Colors (black, green, maroon, rayWhite)
import Types
import Radon.Access.Raylib
import Radon.Access.Class (Freeable(freeDependents), rcalloc, RStorable (rpoke, rsizeOf), rclearBytes)

texPath :: String
texPath = (if not inGHCi then "../../../../../../../../../" else "./") ++ "radon-base/bunnymark/wabbit_alpha.png"

maxBunnies :: Int
maxBunnies = 500000

startup :: IO (Ptr AppState)
startup = do
  _ <- initWindow 800 450 "raylib [textures] example - bunnymark"
  setTargetFPS 60
  unless inGHCi (void $ changeDirectory =<< getApplicationDirectory)
  texPtr <- withCString texPath c'loadTexture
  tWidth <- [access|read Texture texPtr.width|]
  tHeight <- [access|read Texture texPtr.height|]
  statePtr <- rcalloc
  rpoke
    statePtr
    ( AppState
        { texBunny = texPtr,
          bunnies = [],
          halfTexWidth = fromIntegral tWidth / 2,
          halfTexHeight = fromIntegral tHeight / 2
        }
    )
  return statePtr

mainLoop :: Ptr AppState -> IO (Ptr AppState)
mainLoop state = do
  screenWidth <- getScreenWidth
  screenHeight <- getScreenHeight

  clearBunnies <- isKeyPressed KeyC
  when (clearBunnies) $ do
    rclearBytes (rsizeOf (undefined :: Bunny) * 500000) =<< [access|run AppState state.bunnies|]
    [access|lwrite AppState state.bunnies|] 0

  count <- [access|lread AppState state.bunnies|]

  beginDrawing
  clearBackground rayWhite
  forM_
    [0 .. count - 1]
    ( \(!i) ->
        do
          _px <- [access|read AppState state.bunnies[$i].px|]
          _py <- [access|read AppState state.bunnies[$i].py|]
          _color <- [access|run AppState state.bunnies[$i].color|]
          _texBunny <- [access|read AppState state.texBunny|]
          c'drawTexture _texBunny (floor _px) (floor _py) _color
    )
  drawRectangle 0 0 screenWidth 40 black
  drawText ("bunnies: " ++ show count) 120 10 20 green
  drawText ("batched draw calls: " ++ show (1 + (count `div` 8192))) 320 10 20 maroon
  drawFPS 10 10
  endDrawing

  _halfTexWidth <- [access|read AppState state.halfTexWidth|]
  _halfTexHeight <- [access|read AppState state.halfTexHeight|]

  forM_
    [0 .. count - 1]
    ( \(!i) ->
        do
          _px <- [access|read AppState state.bunnies[$i].px|]
          _py <- [access|read AppState state.bunnies[$i].py|]
          _sx <- [access|read AppState state.bunnies[$i].sx|]
          _sy <- [access|read AppState state.bunnies[$i].sy|]
          _color <- [access|read AppState state.bunnies[$i].color|]
          let px' = _px + _sx
              py' = _py + _sy
              adjX = px' + _halfTexWidth
              adjY = py' + _halfTexHeight
          [access|write AppState state.bunnies[$i].px|] px'
          [access|write AppState state.bunnies[$i].py|] py'
          when (adjX > fromIntegral screenWidth || adjX < 0) $ [access|write AppState state.bunnies[$i].sx|] (-_sx)
          when (adjY > fromIntegral screenHeight || adjY < 40) $ [access|write AppState state.bunnies[$i].sy|] (-_sy)
    )

  do
    lDown <- isMouseButtonDown MouseButtonLeft
    if lDown
      then do
        frameTime <- getFrameTime
        let newBunnies = min (round (30000 * frameTime)) (maxBunnies - count)
        mx <- realToFrac <$> c'getMouseX
        my <- realToFrac <$> c'getMouseY
        [access|lwrite AppState state.bunnies|] (count + newBunnies)
        forM_
          [count .. (count + newBunnies - 1)]
          ( \(!i) ->
              do
                xSpeed <- (/ 60) . fromIntegral <$> c'getRandomValue (-250) 250
                ySpeed <- (/ 60) . fromIntegral <$> c'getRandomValue (-250) 250
                r <- fromIntegral <$> c'getRandomValue 50 240
                g <- fromIntegral <$> c'getRandomValue 80 240
                b <- fromIntegral <$> c'getRandomValue 100 240

                [access|write AppState state.bunnies[$i].px|] mx
                [access|write AppState state.bunnies[$i].py|] my
                [access|write AppState state.bunnies[$i].sx|] xSpeed
                [access|write AppState state.bunnies[$i].sy|] ySpeed
                [access|write AppState state.bunnies[$i].color|] (Color r g b 255)
          )
        return state
      else return state

shouldClose :: Ptr AppState -> IO Bool
shouldClose _ = windowShouldClose

teardown :: Ptr AppState -> IO ()
teardown state = do
  c'unloadTexture =<< [access|read AppState state.texBunny|]
  freeDependents state
  free state

raylibApplication 'startup 'mainLoop 'shouldClose 'teardown
