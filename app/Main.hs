{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Main where

import Data.Monoid((<>))
import Control.Monad (forever)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Network.WebSockets (acceptRequest, receiveData, sendTextData, defaultConnectionOptions, PendingConnection)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Graphics.X11.Xlib as X
import qualified XTest as X
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Read (signed, decimal)

ok = return ""

mouseDelta display window dx dy = X.selectInput display window X.keyReleaseMask >> X.warpPointer display 0 0 0 0 0 0 dx dy >> X.flush display >> ok

mouseButton display button mode = X.fakeButtonEvent display button (mode == "down") 0 >> X.flush display >> ok

mouseScroll display d = mapM_ (\_ -> btn True >> btn False) [1..abs d] >> X.flush display >> ok
  where btn s = X.fakeButtonEvent display (if d < 0 then 4 else 5) s 0

keyAction display down code = X.fakeKeyEvent display code down 0 >> X.flush display >> ok

int int = case signed decimal int of
            Right (n, "") -> Right n
            _             -> Left $ "'" <> int <> "' is not a valid integer value"

voicelist = do
  let voices = "{\"type\": \"voicelist\", \"voices\": {\"voz1\": \"una voz muy bonita\", \"voz2\": \"una voz muy fea\"}}"
  return voices

jstr = Text.replace "\"" "\\\""

voiceok = return "{\"type\": \"result\", \"success\": true, \"message\": \"\"}"

voicecommand cmd = do
  Text.putStrLn cmd
  voiceok

digestX11 display window message = case Text.split (','==) message of
  ["mouse"    , dx, dy]     -> mouseDelta display window <$> int dx <*> int dy
  ["leftbtn"  , mode]       -> Right $ mouseButton display 1 mode
  ["middlebtn", mode]       -> Right $ mouseButton display 2 mode
  ["rightbtn" , mode]       -> Right $ mouseButton display 3 mode
  ["scroll"   , d]          -> mouseScroll display <$> int d
  ["key"      , mode, code] -> keyAction display (mode == "down") <$> int code
  ["voicecommand", cmd]     -> Right $ voicecommand cmd
  ["voicelist"]             -> Right $ voicelist
  _                         -> Left $ "{\"type\": \"error\", \"message\": \"cannot parse message: " <> jstr message <> "\"}"

appSockets :: (Text -> Either Text (IO Text)) -> PendingConnection -> IO ()
appSockets digest iconn = acceptRequest iconn >>= \conn -> forever $ digest <$> receiveData conn >>= \case
  Right action -> action >>= sendTextData conn
  Left  error  -> Text.putStrLn error >> sendTextData conn error

appStatic = staticApp $ defaultWebAppSettings "."

main :: IO ()
main = do
  display <- X.openDisplay ":0"
  window  <- X.rootWindow display 0
  run 9035 $ websocketsOr defaultConnectionOptions (appSockets $ digestX11 display window) appStatic
