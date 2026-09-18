module Main (main) where

import Hydra.Pleco.Server.EvalSpec qualified as EvalSpec
import Hydra.Pleco.Server.ProjectSpec qualified as ProjectSpec
import Hydra.Pleco.ServerSpec qualified as ServerSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
  ServerSpec.spec
  ProjectSpec.spec
  EvalSpec.spec
