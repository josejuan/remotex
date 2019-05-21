{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Monoid((<>))
import Control.Monad (forever)

import Network.Wai.Handler.Warp       (run)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Network.WebSockets             (acceptRequest, receiveData, sendTextData, defaultConnectionOptions, PendingConnection)
import Network.Wai.Handler.WebSockets (websocketsOr)

import qualified Graphics.X11.Xlib as X11
import qualified XTest             as X11

import           Data.Text        (Text)
import qualified Data.Text      as Text
import qualified Data.Text.IO   as Text
import           Data.Text.Read   (signed, decimal)

mouseDelta display window dx dy = do
  X11.selectInput display window X11.keyReleaseMask
  X11.warpPointer display 0 0 0 0 0 0 dx dy
  X11.flush display

mouseButton display button mode = do
  X11.xFakeButtonEvent display button (mode == "down") 0
  X11.flush display

mouseScroll display d = do
  let button = if d < 0 then 4 else 5
  mapM_ (\_ -> X11.xFakeButtonEvent display button True  0
            >> X11.xFakeButtonEvent display button False 0) [1..abs d]
  X11.flush display

keyAction display down code = do
  X11.xFakeKeyEvent display code down 0
  X11.flush display

int int = case signed decimal int of
            Right (n, "") -> Right n
            _             -> Left $ "'" <> int <> "' is not a valid integer value"

digestX11 display window message = case Text.split (','==) message of
  ["mouse"    , dx, dy]     -> mouseDelta display window <$> int dx <*> int dy
  ["leftbtn"  , mode]       -> Right $ mouseButton display 1 mode
  ["middlebtn", mode]       -> Right $ mouseButton display 2 mode
  ["rightbtn" , mode]       -> Right $ mouseButton display 3 mode
  ["scroll"   , d]          -> mouseScroll display <$> int d
  ["key"      , mode, code] -> keyAction display (mode == "down") <$> int code
  _                         -> Left $ "cannot parse message: " <> message

appSockets :: (Text -> Either Text (IO ())) -> PendingConnection -> IO ()
appSockets digest iconn = acceptRequest iconn >>= \conn -> forever $ digest <$> (receiveData conn >>= \m -> Text.putStrLn m >> return m) >>= \case
  Right action -> action
  Left  error  -> Text.putStrLn error >> sendTextData conn error

appStatic = staticApp $ defaultWebAppSettings "."

main :: IO ()
main = do
  display <- X11.openDisplay ":0"
  window  <- X11.rootWindow display 0
  run 9035 $ websocketsOr defaultConnectionOptions (appSockets $ digestX11 display window) appStatic
