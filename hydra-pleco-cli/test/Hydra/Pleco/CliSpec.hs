module Hydra.Pleco.CliSpec (spec) where

import Hydra.Pleco.Client
import Hydra.Pleco.Client.Gen qualified as Gen

import Servant.Client (ClientEnv (..))
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, forAll, tripping)

spec :: Spec
spec = describe "Hydra.Pleco.Client" $ do
  describe "mkPlecoClientEnv" $ do
    it "sets expected BaseUrl" $ do
      let baseUrl' = BaseUrl Http "localhost" 8081 ""
      env <- mkPlecoClientEnv baseUrl'

      baseUrl (pceClientEnv env) `shouldBe` baseUrl'

  describe "JobsetSpec" $ do
    it "round-trips through parseJobset" $ 
      hedgehog $ do
        jsSpec <- forAll Gen.jobsetSpec
        tripping jsSpec renderJobsetSpec parseJobsetSpec
