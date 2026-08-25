module Hydra.Pleco.Server
  ( PlecoServerT (..),
    PlecoServerEnv (..),
    runPlecoServerT,
    mkPlecoServerEnv,
    runServer,
  ) where

import Hydra.Pleco.Api (Health (..), HydraApi (..), HydraApp (..))
import Hydra.Pleco.Api qualified as Api
import Hydra.Pleco.Server.Monad

import Hydra.Pleco.Server.DB (releaseConnectionPool, testConnection)
import Hydra.Pleco.Server.Webhook (webhooksHandler, watchHydraEvents)
import Katip qualified
import Network.Wai.Handler.Warp (Port, run)
import Servant
import Servant.Server.Generic (genericServeT)
import Servant.Swagger.UI (swaggerSchemaUIServerT)
import UnliftIO (bracket_)
import UnliftIO.Async qualified as Async
import Hasql.Pool (Pool)

runServer :: Port -> PlecoServerEnv -> IO ()
runServer port env = runPlecoServerT env $ do
  Async.mapConcurrently_
    id
    [ runApp port,
      runHydraWatcher
    ]

runApp :: Port -> PlecoServerT IO ()
runApp port = do
  env@PlecoServerEnv{pseDbPool} <- ask

  Katip.logFM Katip.InfoS $ "Starting pleco-server at http://localhost:" <> show port
  bracket_ 
    (init' pseDbPool) 
    (finalize pseDbPool) 
    (liftIO $ run port (app env))
  where
    init' :: Pool -> PlecoServerT IO ()
    init' dbPool = do
      -- Test connecting to the database, log and fail on exception
      testConnection dbPool `Katip.logExceptionM` Katip.ErrorS
      Katip.logFM Katip.InfoS $ "Connection to database '" <> "dbname=hydra" <> "' successful"

    finalize :: Pool -> PlecoServerT IO ()
    finalize dbPool = do
      Katip.logFM Katip.InfoS $ "Stopped pleco-server at http://localhost:" <> show port
      releaseConnectionPool dbPool

runHydraWatcher :: PlecoServerT IO ()
runHydraWatcher = do
  Katip.logFM Katip.InfoS "Starting Hydra watcher at db=hydra"
  watchHydraEvents

app :: PlecoServerEnv -> Application
app env = genericServeT (runPlecoServerT env) server

server :: ServerT (NamedRoutes HydraApp) (PlecoServerT Handler)
server =
  HydraApp
    { api = apiServer,
      docs = swaggerSchemaUIServerT Api.hydraOpenApi
    }

apiServer :: ServerT (NamedRoutes HydraApi) (PlecoServerT Handler)
apiServer =
  HydraApi
    { health = healthHandler,
      webhooks = webhooksHandler
    }

healthHandler :: PlecoServerT Handler Health
healthHandler = pure (Health "pass")
