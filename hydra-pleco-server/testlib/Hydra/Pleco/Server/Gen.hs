module Hydra.Pleco.Server.Gen
  ( project,
    jobsetEval,
  ) where

import Hydra.Pleco.Server.DB.Hydra
  ( JobsetEval (..),
    JobsetEvalId (..),
    JobsetId (..),
    Project (..),
  )

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

jobsetEval :: Gen (JobsetEval Result)
jobsetEval = do
  id' <- JobsetEvalId <$> genId
  jobsetId <- JobsetId <$> genId
  timestamp <- genId
  checkoutTime <- genId
  evalTime <- genId
  -- Hydra stores hasnewbuilds as a 0/1 flag
  hasNewBuilds <- Gen.element [0, 1]
  hash' <- genTitle
  numBuilds <- Gen.maybe genId
  numSucceeded <- Gen.maybe genId
  flake <- Gen.maybe genUrl

  pure
    JobsetEval
      { jseId = id',
        jseJobsetId = jobsetId,
        jseTimestamp = timestamp,
        jseCheckoutTime = checkoutTime,
        jseEvalTime = evalTime,
        jseHasNewBuilds = hasNewBuilds,
        jseHash = hash',
        jseNumBuilds = numBuilds,
        jseNumSucceeded = numSucceeded,
        jseFlake = flake
      }

genId :: Gen Int64
genId = Gen.int64 (Range.linear 0 maxBound)

genTitle :: Gen Text
genTitle = Gen.text (Range.linear 0 255) Gen.unicode

genUrl :: Gen Text
genUrl = Gen.text len chars
  where
    -- maximum length of URL is essentially 2048
    len = Range.linear 0 2048
    -- latin1 should cover any URL-encoded string
    chars = Gen.latin1
