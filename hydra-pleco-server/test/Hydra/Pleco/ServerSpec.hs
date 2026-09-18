module Hydra.Pleco.ServerSpec (spec) where

import Hydra.Pleco.Api (Health (..), Subscription (..))
import Hydra.Pleco.Api qualified as Api
import Hydra.Pleco.Client
  ( PlecoClient,
    getHealth,
    mkPlecoClientEnv,
    plecoClient,
    runPlecoClient,
  )
import Hydra.Pleco.Server (PlecoServerEnv (..), mkPlecoServerEnv, runPlecoServerT)
import Hydra.Pleco.Server.DB (newConnectionPool)
import Hydra.Pleco.Server.TestApp (mkTestEnv, withPlecoApp)

import Network.HTTP.Client
  ( Request (..),
    RequestBody (..),
    Response,
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
    responseStatus,
  )
import Network.HTTP.Types (statusCode)
import Network.Wai.Handler.Warp (Port)
import Servant.Client (BaseUrl (..), Scheme (Http), (//))
import Test.Hspec

spec :: Spec
spec = do
  describe "runPlecoServerT" $
    it "runs an action in the reader environment" $ do
      env <- mkPlecoServerEnv "" []
      ns <- runPlecoServerT env (asks pseLogNamespace)
      pseLogNamespace env `shouldBe` ns

  describe "GET /health" $ do
    it "reports pass" $
      withServer [] $ \port ->
        client port getHealth `shouldReturn` Health "pass"

    it "returns 406 when client does not accept health+json" $
      withServer [] $ \port -> do
        let req' r = r {requestHeaders = [("Accept", "application/xml")]}

        resp <- withRequest port "/health" req'
        statusCode (responseStatus resp) `shouldBe` 406

  describe "POST /webhooks" $ do
    it "returns the registered subscription" $
      withServer [] $ \port ->
        client port (subscribe subscription) `shouldReturn` subscription

    it "registers subscriptions" $
      withServer [] $ \port -> do
        _ <- client port (subscribe subscription)
        client port listSubscriptions `shouldReturn` [subscription]

    it "returns 400 on malformed body" $
      withServer [] $ \port -> do
        let req' r =
              r
                { method = "POST",
                  requestBody = RequestBodyLBS "{\"not-a-url\":true}",
                  requestHeaders = [("Content-Type", "application/json")]
                }

        resp <- withRequest port "/webhooks" req'
        statusCode (responseStatus resp) `shouldBe` 400

  describe "GET /webhooks" $
    it "lists preseeded subscriptions" $
      withServer [subscription] $ \port ->
        client port listSubscriptions `shouldReturn` [subscription]

  it "returns 404 on unknown route" $
    withServer [] $ \port -> do
      resp <- runRequest port "/nope"
      statusCode (responseStatus resp) `shouldBe` 404

withServer :: [Subscription] -> (Port -> IO a) -> IO a
withServer subs act = do
  let connInfo = ""
  pool <- newConnectionPool connInfo
  env <- mkTestEnv connInfo pool subs
  withPlecoApp env act

client :: Port -> PlecoClient a -> IO a
client port act = do
  env <- mkPlecoClientEnv (BaseUrl Http "localhost" port "")
  runPlecoClient env act

subscription :: Subscription
subscription = Subscription "http://localhost:9999/"

subscribe :: Subscription -> PlecoClient Subscription
subscribe = plecoClient // Api.webhooks // Api.subscribe

runRequest :: Port -> String -> IO (Response LByteString)
runRequest port path = withRequest port path id

withRequest :: Port -> String -> (Request -> Request) -> IO (Response LByteString)
withRequest port path f = do
  manager <- newManager defaultManagerSettings
  req <- parseRequest ("http://localhost:" <> show port <> path)
  httpLbs (f req) manager

listSubscriptions :: PlecoClient [Subscription]
listSubscriptions = plecoClient // Api.webhooks // Api.list
