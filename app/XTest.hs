{-# LANGUAGE ForeignFunctionInterface #-}
module XTest where

import Graphics.X11.Xlib
import Foreign
import Foreign.C.Types

foreign import ccall unsafe "X11/extensions/XTest.h XTestFakeButtonEvent"
    xFakeButtonEvent :: Display -> Button -> Bool -> Time -> IO Status

foreign import ccall unsafe "X11/extensions/XTest.h XTestFakeMotionEvent"
    xFakeMotionEvent :: Display -> CInt -> CInt -> CInt -> Time -> IO Status

foreign import ccall unsafe "X11/extensions/XTest.h XTestFakeKeyEvent"
    xFakeKeyEvent :: Display -> KeyCode -> Bool -> CULong -> IO Status
