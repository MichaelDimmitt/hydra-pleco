module Hydra.Pleco.Server.Evals
  ( evalsHandler,
    listEvalsHandler,
  ) where

import Hydra.Pleco.Api (Eval (..), EvalId (..), EvalsApi (..), JobsetId (..))
import Hydra.Pleco.Server.DB qualified as DB
import Hydra.Pleco.Server.Evals.DB (fromEval)
import Hydra.Pleco.Server.Monad (PlecoServerEnv (..), PlecoServerT)

import Servant (Handler, NamedRoutes, ServerT)

evalsHandler :: ServerT (NamedRoutes EvalsApi) (PlecoServerT Handler)
evalsHandler =
  EvalsApi
    { evaGet = getEvalHandler
    }

listEvalsHandler :: JobsetId -> PlecoServerT Handler [Eval]
listEvalsHandler (JobsetId jobsetId) = do
  pool <- asks pseDbPool
  evals <-
    DB.runSession pool $
      DB.statement () $
        DB.jobsetEvalsByJobset (DB.JobsetId $ fromIntegral jobsetId)

  pure $ map fromEval evals

getEvalHandler :: EvalId -> PlecoServerT Handler Eval
getEvalHandler (EvalId evalId) = do
  pool <- asks pseDbPool
  eval <-
    DB.runSession pool $
      DB.statement () (DB.jobsetEvalById evalId')

  pure $ fromEval eval
  where
    evalId' = DB.JobsetEvalId (fromIntegral evalId)
