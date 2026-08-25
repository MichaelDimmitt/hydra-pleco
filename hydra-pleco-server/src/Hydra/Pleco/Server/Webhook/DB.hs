module Hydra.Pleco.Server.Webhook.DB
  ( toHydraNotification,
    fromHydraNotification,
  ) where

import Hydra.Pleco.Api
import Hydra.Pleco.Server.DB (HydraNotification(..))

-- TODO[sgillespie]: Implement me
toHydraNotification :: JobsetEvent -> HydraNotification
toHydraNotification JobsetEvent{..} = undefined

-- TODO[sgillespie]: Implement me
fromHydraNotification :: HydraNotification -> JobsetEvent
fromHydraNotification event = 
  let eventTy =
        case event of
          HydraEvalAdded _ _ -> EvalAdded
          HydraEvalStarted _ -> EvalStarted
          HydraEvalCached _ _ -> EvalCached
          HydraEvalFailed _ -> EvalFailed
  in  JobsetEvent
        { jeEventType = eventTy,
          jeProject = Project "",
          jeJobset = Jobset ""
        }
