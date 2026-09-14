module Hydra.Pleco.Client
  ( -- * The Pleco API client monad
    PlecoClient (..),
    PlecoClientEnv (..),
    PlecoClientError (..),
    mkPlecoClientEnv,
    runPlecoClient,
    plecoClient,

    -- * Querying the Pleco API
    getHealth,
    getProject,
    listProjects,
    listJobsets,
    getJobset,

    -- * Running the reference webhook server
    runEchoServer,

    -- * Pleco API interface types
    Api.HealthJSON,
    Api.Health (..),
    Api.Subscription (..),
    Api.JobsetEvent (..),
    Api.EventType (..),
    Api.Project (..),
    Api.ProjectId (..),
    Api.Jobset (..),
    Api.JobsetId (..),
    Api.JobsetName (..),
    Api.JobsetType (..),

    -- * Re-exports
    BaseUrl (..),
    Servant.Scheme (..),
    Servant.parseBaseUrl,
    FromJSON,
    ToJSON,
    Aeson.encodePretty,
  ) where

import Hydra.Pleco.Api (Health, HydraApi, Jobset, Project, ProjectId)
import Hydra.Pleco.Api qualified as Api
import Hydra.Pleco.Client.EchoServer (runEchoServer)

import Control.Exception (throwIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Encode.Pretty qualified as Aeson
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.Client (AsClientT, BaseUrl, ClientEnv, ClientError, ClientM, (//), (/:))
import Servant.Client qualified as Servant
import Servant.Client.Generic (genericClientHoist)

-- | Client application monad stack
newtype PlecoClient a = PlecoClient {unPlecoClient :: ReaderT PlecoClientEnv ClientM a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader PlecoClientEnv,
      MonadIO
    )

newtype PlecoClientEnv = PlecoClientEnv
  { pceClientEnv :: ClientEnv
  }

newtype PlecoClientError = PlecoClientError ClientError
  deriving stock (Eq, Show)
  deriving newtype (Exception)

mkPlecoClientEnv :: BaseUrl -> IO PlecoClientEnv
mkPlecoClientEnv url = do
  manager <- newManager defaultManagerSettings
  pure $ PlecoClientEnv (Servant.mkClientEnv manager url)

runPlecoClient :: PlecoClientEnv -> PlecoClient a -> IO a
runPlecoClient env@PlecoClientEnv {..} action = do
  res <- Servant.runClientM (runReaderT (unPlecoClient action) env) pceClientEnv
  either throwIO pure res

plecoClient :: HydraApi (AsClientT PlecoClient)
plecoClient = genericClientHoist hoistClientM

hoistClientM :: ClientM a -> PlecoClient a
hoistClientM = PlecoClient . lift

getHealth :: PlecoClient Health
getHealth = plecoClient // Api.health

listProjects :: PlecoClient [Project]
listProjects = plecoClient // Api.projects // Api.listProjects

getProject :: ProjectId -> PlecoClient Project
getProject = plecoClient // Api.projects // Api.getProject

listJobsets :: ProjectId -> PlecoClient [Jobset]
listJobsets projectId =
  plecoClient
    // Api.projects
    // Api.jobsets
    /: projectId
    // Api.listJobsets

getJobset :: ProjectId -> Api.JobsetName -> PlecoClient Jobset
getJobset projectId =
  plecoClient
    // Api.projects
    // Api.jobsets
    /: projectId
    // Api.getJobset
