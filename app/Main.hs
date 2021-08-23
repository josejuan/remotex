{-# LANGUAGE OverloadedStrings, LambdaCase, RecordWildCards #-}
module Main where

import Data.Monoid((<>))
import Data.Maybe
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
import Data.Text.ICU (normalize, NormalizationMode(NFD))
import Data.Text.ICU.Char (property, Bool_(Diacritic))
import System.Directory (listDirectory, getHomeDirectory)
import System.Process
import System.Exit

ok = return ""

mouseDelta display window dx dy = X.selectInput display window X.keyReleaseMask >> X.warpPointer display 0 0 0 0 0 0 dx dy >> X.flush display >> ok

mouseButton display button mode = X.fakeButtonEvent display button (mode == "down") 0 >> X.flush display >> ok

mouseScroll display d = mapM_ (\_ -> btn True >> btn False) [1..abs d] >> X.flush display >> ok
  where btn s = X.fakeButtonEvent display (if d < 0 then 4 else 5) s 0

keyAction display down code = X.fakeKeyEvent display code down 0 >> X.flush display >> ok

int int = case signed decimal int of
            Right (n, "") -> Right n
            _             -> Left $ "'" <> int <> "' is not a valid integer value"

data OutputMode = AsText | AsHtml | NoOutput deriving Show

data Voice = Voice { voice_file :: FilePath
                   , voice_name :: Text
                   , voice_description :: Text
                   , voice_outputmode :: OutputMode } deriving Show

getUserVoices = do
    d <- (<> "/remotex/voicecommands") <$> getHomeDirectory
    let   readvoice f = (parsevoice . Text.lines) <$> Text.readFile f'
                        where f' = d <> "/" <> f
                              parsevoice script = Voice f' <$> getName script
                                                           <*> getDescription script
                                                           <*> getOutputMode script
          get pfx [] = Nothing
          get pfx (x:xs) = if Text.isPrefixOf pfx x then Just (Text.drop (Text.length pfx) x) else get pfx xs
          getName = get "#remotex.title "
          getDescription = get "#remotex.description "
          getOutputMode xs = m <$> get "#remotex.outputmode " xs
                          where m "text" = AsText
                                m "html" = AsHtml
                                m _      = NoOutput
    listDirectory d >>= mapM readvoice >>= return . catMaybes

voicelist = do
    xs <- getUserVoices
    return $ "{\"type\": \"voicelist\", \"voices\": {" <> Text.intercalate "," (map jvoice xs) <> "}}"
  where jvoice Voice { .. } = "\"" <> jstr voice_name <> "\": \"" <> jstr voice_description <> "\""

jstr = Text.replace "\n" "\\n" . Text.replace "\r" "\\r" . Text.replace "\t" "\\t" . Text.replace "\"" "\\\""

result success message html = "{\"type\": \"result\", \"success\": " <> (if success then "true" else "false") <> ", \"message\": \"" <> jstr message <> "\", \"html\": \"" <> jstr html <> "\"}"
resultko message = result False message ""
resultok message = result True message ""
resulthtml = result True ""

normtext = Text.toLower . Text.filter (not . property Diacritic) . normalize NFD

voicecommand cmd = do
  xs <- getUserVoices
  case listToMaybe [v | v <- xs, normtext (voice_name v) == normtext cmd] of
    Nothing -> return $ retko $ "command \"" <> cmd <> "\" not found"
    Just (Voice {..}) -> do
                            (exitcode, stdout, stderr) <- readProcessWithExitCode voice_file [] ""
                            case exitcode of
                              ExitSuccess -> case voice_outputmode of
                                               AsText -> return $ resultok $ Text.pack stdout
                                               AsHtml -> return $ resulthtml $ Text.pack stdout
                                               _      -> return $ resultok ""
                              _           -> return $ resultko $ Text.pack stderr

digestX11 display window message = case Text.split (','==) message of
  ["mouse"    , dx, dy]     -> mouseDelta display window <$> int dx <*> int dy
  ["leftbtn"  , mode]       -> Right $ mouseButton display 1 mode
  ["middlebtn", mode]       -> Right $ mouseButton display 2 mode
  ["rightbtn" , mode]       -> Right $ mouseButton display 3 mode
  ["scroll"   , d]          -> mouseScroll display <$> int d
  ["key"      , mode, code] -> keyAction display (mode == "down") <$> int code
  ["voicecommand", cmd]     -> Right $ voicecommand cmd
  ["voicelist"]             -> Right $ voicelist
  _                         -> Left $ "cannot parse message: " <> message

retko err = "{\"type\": \"error\", \"message\": \"" <> jstr err <> "\"}"

appSockets :: (Text -> Either Text (IO Text)) -> PendingConnection -> IO ()
appSockets digest iconn = acceptRequest iconn >>= \conn -> forever $ digest <$> receiveData conn >>= \case
  Right action -> action >>= sendTextData conn
  Left  error  -> Text.putStrLn (retko error) >> sendTextData conn error

appStatic = staticApp $ defaultWebAppSettings "."

main :: IO ()
main = do
  display <- X.openDisplay ":0"
  window  <- X.rootWindow display 0
  run 9035 $ websocketsOr defaultConnectionOptions (appSockets $ digestX11 display window) appStatic
