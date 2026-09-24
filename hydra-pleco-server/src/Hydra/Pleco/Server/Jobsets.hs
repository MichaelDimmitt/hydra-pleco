module Hydra.Pleco.Server.Jobsets
  ( jobsetsHandler,
    listJobsetsHandler,
  ) where

import Hydra.Pleco.Api (Jobset (..), JobsetId (..), JobsetsApi (..), ProjectId (..))
import Hydra.Pleco.Server.DB (jobsetById, jobsetsByProject, runSession, statement)
import Hydra.Pleco.Server.DB qualified as DB
import Hydra.Pleco.Server.Evals (listEvalsHandler)
import Hydra.Pleco.Server.Jobsets.DB (fromJobset)
import Hydra.Pleco.Server.Monad (PlecoServerEnv (..), PlecoServerT)

import Servant (Handler, NamedRoutes, ServerT)

jobsetsHandler :: ServerT (NamedRoutes JobsetsApi) (PlecoServerT Handler)
jobsetsHandler =
  JobsetsApi
    { jsaGet = getJobsetHandler,
      jsaEvals = listEvalsHandler
    }

listJobsetsHandler :: ProjectId -> PlecoServerT Handler [Jobset]
listJobsetsHandler (ProjectId prjName) = do
  pool <- asks pseDbPool
  jobsets <- runSession pool $ statement () (jobsetsByProject prjName)

  pure $ map fromJobset jobsets

getJobsetHandler :: JobsetId -> PlecoServerT Handler Jobset
getJobsetHandler (JobsetId jobsetId) = do
  pool <- asks pseDbPool
  jobset <-
    runSession pool $
      statement () $
        jobsetById (DB.JobsetId $ fromIntegral jobsetId)

  pure $ fromJobset jobset
