module Hydra.Pleco.Server.Gen
  ( project,
  ) where

import Hydra.Pleco.Server.DB.Hydra (Project (..))

import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Rel8 (Result)

project :: Gen (Project Result)
project = do
  name <- genTitle
  displayName <- genTitle
  description <- Gen.maybe genTitle
  enabled <- Gen.bool
  hidden <- Gen.bool
  owner <- genTitle
  homepage <- Gen.maybe genUrl
  declFile <- Gen.maybe genUrl
  declType <- Gen.maybe genTitle
  enableDynCmd <- Gen.bool

  pure
    Project
      { prjName = name,
        prjDisplayName = displayName,
        prjDescription = description,
        prjEnabled = enabled,
        prjHidden = hidden,
        prjOwner = owner,
        prjHomepage = homepage,
        prjDeclFile = declFile,
        prjDeclType = declType,
        prjEnableDynCmd = enableDynCmd
      }

genTitle :: Gen Text
genTitle = Gen.text (Range.linear 0 255) Gen.unicode

genUrl :: Gen Text
genUrl = Gen.text len chars
  where
    -- maximum length of URL is essentially 2048
    len = Range.linear 0 2048
    -- latin1 should cover any URL-encoded string
    chars = Gen.latin1
