module Hydra.Pleco.Server.Evals
  ( evalsHandler,
  ) where

import Hydra.Pleco.Api (Eval (..), EvalId (..), EvalsApi (..), JobsetName (..), ProjectId (..))
import Hydra.Pleco.Server.Monad (PlecoServerEnv (..), PlecoServerT)

import Hydra.Pleco.Server.DB
  ( JobsetEvalId (..),
    jobsetEvalByProjectAndJobset,
    jobsetEvalsByProjectAndJobset,
    runSession,
    statement,
  )
import Hydra.Pleco.Server.Evals.DB (fromEval)
import Servant (Handler, NamedRoutes, ServerT)

evalsHandler
  :: ProjectId
  -> JobsetName
  -> ServerT (NamedRoutes EvalsApi) (PlecoServerT Handler)
evalsHandler projectId jobsetName =
  EvalsApi
    { listEvals = listEvalsHandler projectId jobsetName,
      getEval = getEvalHandler projectId jobsetName
    }

listEvalsHandler :: ProjectId -> JobsetName -> PlecoServerT Handler [Eval]
listEvalsHandler (ProjectId prjName) (JobsetName jobsetName) = do
  pool <- asks pseDbPool
  evals <-
    runSession pool $
      statement () (jobsetEvalsByProjectAndJobset prjName jobsetName)

  pure $ map fromEval evals

getEvalHandler :: ProjectId -> JobsetName -> EvalId -> PlecoServerT Handler Eval
getEvalHandler (ProjectId prjName) (JobsetName jobsetName) (EvalId evalId) = do
  pool <- asks pseDbPool
  eval <-
    runSession pool $
      statement () (jobsetEvalByProjectAndJobset prjName jobsetName evalId')

  pure $ fromEval eval
  where
    evalId' = JobsetEvalId (fromIntegral evalId)
