module Hydra.Pleco.Server.Jobsets
  ( jobsetsHandler,
  ) where

import Hydra.Pleco.Api (Jobset (..), JobsetName (..), JobsetsApi (..), ProjectId (..))
import Hydra.Pleco.Server.Monad (PlecoServerEnv (..), PlecoServerT)

import Hydra.Pleco.Server.DB (jobsetByProjectAndName, jobsetsByProject, runSession, statement)
import Hydra.Pleco.Server.Jobsets.DB (fromJobset)
import Servant (Handler, NamedRoutes, ServerT)

jobsetsHandler :: ProjectId -> ServerT (NamedRoutes JobsetsApi) (PlecoServerT Handler)
jobsetsHandler projectId =
  JobsetsApi
    { listJobsets = listJobsetsHandler projectId,
      getJobset = getJobsetHandler projectId
    }

listJobsetsHandler :: ProjectId -> PlecoServerT Handler [Jobset]
listJobsetsHandler (ProjectId prjName) = do
  pool <- asks pseDbPool
  jobsets <- runSession pool $ statement () (jobsetsByProject prjName)

  pure $ map fromJobset jobsets

getJobsetHandler :: ProjectId -> JobsetName -> PlecoServerT Handler Jobset
getJobsetHandler (ProjectId prjName) (JobsetName jobsetName) = do
  pool <- asks pseDbPool
  jobset <-
    runSession pool $
      statement () (jobsetByProjectAndName prjName jobsetName)

  pure $ fromJobset jobset
