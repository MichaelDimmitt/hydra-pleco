module Hydra.Pleco.Client.Gen
  ( jobsetSpec,
  ) where

import Hydra.Pleco.Api.Gen qualified as ApiGen
import Hydra.Pleco.Client (JobsetSpec (..))

import Hedgehog (Gen)

jobsetSpec :: Gen JobsetSpec
jobsetSpec = JobsetSpec <$> ApiGen.projectId <*> ApiGen.jobsetName
