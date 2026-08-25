module Hydra.Pleco.Server.Webhook
  ( webhooksHandler,
    webhooksSubscribeHandler,
    webhooksListHandler,
    watchHydraEvents,
    toHydraNotification,
    fromHydraNotification,
  ) where

import Hydra.Pleco.Api
import Hydra.Pleco.Server.DB (HydraNotification, hydraNotifyChannels, newConnection, parseHydraNotification, releaseConnection)
import Hydra.Pleco.Server.Monad (PlecoServerEnv (..), PlecoServerT)
import Hydra.Pleco.Server.Webhook.DB (fromHydraNotification, toHydraNotification)

import Hasql.Notifications (listen, toPgIdentifier, waitForNotifications)
import Katip qualified
import Servant (Handler, HasServer (..), NamedRoutes)
import Servant.Client (AsClientT, ClientError, ClientM, (//), (/:))
import Servant.Client qualified as Servant
import Servant.Client.Generic (genericClient)
import UnliftIO (MonadUnliftIO (..), bracket, modifyTVar)

webhooksHandler :: ServerT (NamedRoutes WebhooksApi) (PlecoServerT Handler)
webhooksHandler =
  WebhooksApi
    { subscribe = webhooksSubscribeHandler,
      list = webhooksListHandler
    }

webhooksSubscribeHandler :: Subscription -> PlecoServerT Handler Subscription
webhooksSubscribeHandler sub = do
  subs <- asks pseSubscriptions
  atomically $ modifyTVar subs (sub :)
  pure sub

webhooksListHandler :: PlecoServerT Handler [Subscription]
webhooksListHandler = readTVarIO =<< asks pseSubscriptions

watchHydraEvents :: PlecoServerT IO ()
watchHydraEvents = do
  bracket newConnection releaseConnection $ \conn ->
    withRunInIO $ \run -> do
      -- Register this session as listener to hydra-notify channels (eval_started,
      -- eval_added, etc)
      mapM_ (listen conn . toPgIdentifier) hydraNotifyChannels
      -- Wait for notifications and process them one at a time
      flip waitForNotifications conn $ \chan payload -> run $ do
        let notification = parseHydraNotification (decodeUtf8 chan) (decodeUtf8 payload)
        case notification of
          Left err -> Katip.logFM Katip.ErrorS $ Katip.ls err
          Right parsed -> handleNotification parsed

handleNotification :: HydraNotification -> PlecoServerT IO ()
handleNotification notification = do
  Katip.logFM Katip.InfoS $
    "Received Hydra event: " <> show notification

  PlecoServerEnv {pseSubscriptions} <- ask
  subs <- readTVarIO pseSubscriptions

  let event = fromHydraNotification notification
  forM_ subs $ \sub -> do
    res <- sendWebhook event sub
    case res of
      Left err ->
        Katip.logFM Katip.WarningS $ "Received invalid response: " <> Katip.showLS err
      Right () ->
        Katip.logFM Katip.InfoS $ "Successfully sent webhook to " <> Katip.showLS sub

sendWebhook :: JobsetEvent -> Subscription -> PlecoServerT IO (Either ClientError ())
sendWebhook reqPayload (Subscription url) = do
  manager' <- asks pseClientManager
  liftIO . runExceptT $ do
    baseUrl <- Servant.parseBaseUrl (toString url)
    let env' = Servant.mkClientEnv manager' baseUrl
    ExceptT $ Servant.runClientM (postWebhook reqPayload) env'

postWebhook :: JobsetEvent -> ClientM ()
postWebhook event = webhookClient // webhook /: event

webhookClient :: JobsetEventApi (AsClientT ClientM)
webhookClient = genericClient

