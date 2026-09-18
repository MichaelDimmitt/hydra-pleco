module Main (main) where

import Hydra.Pleco.Api.OpenApiSpec qualified as OpenApiSpec
import Hydra.Pleco.ApiSpec qualified as ApiSpec

import Test.Hspec

main :: IO ()
main = hspec $ do
  ApiSpec.spec
  OpenApiSpec.spec
