module Hydra.Pleco.Client.EchoServer
  ( runEchoServer,
    jobsetEventApp,
  ) where

import Hydra.Pleco.Api.Event (JobsetEvent, JobsetEventApi (..))

import Data.Aeson.Encode.Pretty qualified as AesonPretty
import Katip (KatipContext, KatipContextT, LogEnv)
import Katip qualified
import Network.Wai (Application)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified as Warp
import Servant.Server.Generic (genericServe)

-- | Serve the jobset event callback on @127.0.0.1@, logging what arrives
runEchoServer :: Port -> IO ()
runEchoServer port = do
  logEnv <- mkLogEnv
  let runLog :: KatipContextT IO a -> IO a
      runLog = Katip.runKatipContextT logEnv () "echo"

  runLog . Katip.logFM Katip.InfoS $
    "Listening for jobset events at http://127.0.0.1:" <> show port

  Warp.runSettings (settings port) . jobsetEventApp $ runLog . logEvent

-- | Reference implementation of the jobset event callback. The handler is the
-- reuse point: logging for @pleco echo@, 'putMVar' for tests.
jobsetEventApp :: (JobsetEvent -> IO ()) -> Application
jobsetEventApp onEvent = genericServe JobsetEventApi {webhook = liftIO . onEvent}

logEvent :: (KatipContext m) => JobsetEvent -> m ()
logEvent event =
  Katip.logFM Katip.InfoS $
    "Received jobset event:\n" <> Katip.ls (decodeUtf8 @Text (AesonPretty.encodePretty event))

settings :: Port -> Warp.Settings
settings port =
  Warp.setHost "127.0.0.1"
    . Warp.setPort port
    $ Warp.defaultSettings

mkLogEnv :: IO LogEnv
mkLogEnv = do
  logEnv <- Katip.initLogEnv "pleco" "production"
  scribe <-
    Katip.mkHandleScribe
      Katip.ColorIfTerminal
      stderr
      (Katip.permitItem Katip.InfoS)
      Katip.V2
  Katip.registerScribe "stderr" scribe Katip.defaultScribeSettings logEnv
