module Hydra.Pleco.Api.Gen
  ( health,
    subscription,
    jobsetEvent,
    projectId,
    eventType,
    project,
  ) where

import Hydra.Pleco.Api
  ( EventType (..),
    Health (..),
    Jobset (..),
    JobsetEvent (..),
    Project (..),
    ProjectId (..),
    Subscription (..),
  )

import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

health :: Gen Health
health = Health <$> Gen.element ["pass", "fail", "warn"]

subscription :: Gen Subscription
subscription = Subscription <$> genUrl

jobsetEvent :: Gen JobsetEvent
jobsetEvent = do
  eventType' <- eventType
  project' <- projectId
  jobset' <- jobset

  pure
    JobsetEvent
      { jeEventType = eventType',
        jeProject = project',
        jeJobset = jobset'
      }

eventType :: Gen EventType
eventType =
  Gen.element
    [ EvalStarted,
      EvalAdded,
      EvalCached,
      EvalFailed,
      BuildQueued,
      CachedBuildQueued,
      BuildStarted,
      BuildFinished,
      CachedBuildFinished
    ]

project :: Gen Project
project = do
  name <- projectId
  enabled <- Gen.bool
  visible <- Gen.bool
  displayName <- genTitle
  description <- Gen.maybe genTitle
  homepage <- Gen.maybe genUrl
  owner <- genTitle
  enableDynRunCmd <- Gen.bool
  declSpecFile <- Gen.maybe (toString <$> genUrl)
  declInputType <- Gen.maybe genTitle

  pure
    Project
      { prjId = name,
        prjEnabled = enabled,
        prjVisible = visible,
        prjDisplayName = displayName,
        prjDescription = description,
        prjHomepage = homepage,
        prjOwner = owner,
        prjEnableDynRunCmd = enableDynRunCmd,
        prjDeclSpecFile = declSpecFile,
        prjDeclInputType = declInputType
      }

projectId :: Gen ProjectId
projectId = ProjectId <$> genTitle

jobset :: Gen Jobset
jobset = Jobset <$> genTitle

genTitle :: Gen Text
genTitle = Gen.text (Range.linear 0 255) Gen.unicode

genUrl :: Gen Text
genUrl = Gen.text len chars
  where
    -- maximum length of URL is essentially 2048
    len = Range.linear 0 2048
    -- latin1 should cover any URL-encoded string
    chars = Gen.latin1
