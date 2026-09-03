module Hydra.Pleco.Server.ProjectSpec (spec) where

import Hydra.Pleco.Server.DB.Hydra (Project(..))
import Hydra.Pleco.Server.Gen qualified as Gen
import Hydra.Pleco.Api.Gen qualified as ApiGen
import Hydra.Pleco.Server.Projects.DB (fromProject, toProject)

import Hedgehog (forAll)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog, tripping, (===))
import qualified Hydra.Pleco.Api.Project as Api

spec :: Spec
spec = describe "Project" $ do
  it "round-trips through the Aeson" $
    hedgehog $ do
      prj <- forAll Gen.project
      tripping prj fromProject (Identity . toProject)

  describe "fromProject" $ do
    it "sets visible to not hidden" $
      hedgehog $ do
        prj@Project{prjHidden} <- forAll Gen.project
        Api.prjVisible (fromProject prj) === not prjHidden

  describe "toProject" $
    it "sets hidden to not visible" $ do
      hedgehog $ do
        prj@Api.Project{prjVisible} <- forAll ApiGen.project
        prjHidden (toProject prj) === not prjVisible
