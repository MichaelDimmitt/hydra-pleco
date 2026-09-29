module Hydra.Pleco.Server.Jobsets
  ( jobsetsHandler,
    listJobsetsHandler,
  ) where

import Hydra.Pleco.Api (Jobset (..), JobsetId (..), JobsetName, JobsetsApi (..), ProjectId (..), unJobsetName)
import Hydra.Pleco.Server.DB (jobsetById, runSession, statement)
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

listJobsetsHandler :: ProjectId -> Maybe JobsetName -> PlecoServerT Handler [Jobset]
listJobsetsHandler (ProjectId prjName) jobset = do
  pool <- asks pseDbPool

  let stmt =
        maybe
          (DB.jobsetsByProject prjName)
          (DB.jobsetsByProjectAndName prjName . unJobsetName)
          jobset

  jobsets <- runSession pool $ statement () stmt

  pure $ map fromJobset jobsets

getJobsetHandler :: JobsetId -> PlecoServerT Handler Jobset
getJobsetHandler (JobsetId jobsetId) = do
  pool <- asks pseDbPool
  jobset <-
    runSession pool $
      statement () $
        jobsetById (DB.JobsetId $ fromIntegral jobsetId)

  pure $ fromJobset jobset
