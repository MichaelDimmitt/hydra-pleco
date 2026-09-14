module Hydra.Pleco.Api.Gen
  ( health,
    subscription,
    jobsetEvent,
    eventType,
    project,
    projectId,
    jobset,
    jobsetName,
    jobsetId,
    jobsetState,
    jobsetType,
  ) where

import Hydra.Pleco.Api
  ( EventType (..),
    Health (..),
    Jobset (..),
    JobsetEvent (..),
    JobsetId (..),
    JobsetName (..),
    JobsetState (..),
    JobsetType (..),
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
  jobsetName' <- jobsetName

  pure
    JobsetEvent
      { jeEventType = eventType',
        jeProject = project',
        jeJobset = jobsetName'
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
jobset = do
  name' <- jobsetName
  project' <- projectId
  id' <- jobsetId
  state' <- jobsetState
  visible <- Gen.bool
  jsType <- jobsetType
  flake <- Gen.maybe genUrl
  nixExprInput <- Gen.maybe genUrl
  nixExprPath <- Gen.maybe genTitle
  description <- Gen.maybe genTitle
  checkInterval <- Gen.int (Range.linear 0 maxBound)
  schedulingShares <- Gen.int (Range.linear 0 21)
  enableDynRunCmd <- Gen.bool
  enableEmail <- Gen.bool
  emailOverride <- Gen.maybe genTitle
  keepNumEvals <- Gen.int (Range.linear 0 50)
  lastCheckedTime <- Gen.maybe $ Gen.int (Range.linear 0 maxBound)
  lastEvalTime <- Gen.maybe $ Gen.int (Range.linear 0 maxBound)
  errorMsg <- Gen.maybe genTitle
  errorTime <- Gen.maybe $ Gen.int (Range.linear 0 maxBound)

  pure
    Jobset
      { jsName = name',
        jsProject = project',
        jsId = id',
        jsState = state',
        jsVisible = visible,
        jsType = jsType,
        jsFlake = flake,
        jsNixExprInput = nixExprInput,
        jsNixExprPath = nixExprPath,
        jsDescription = description,
        jsCheckInterval = checkInterval,
        jsSchedulingShares = schedulingShares,
        jsEnableDynRunCmd = enableDynRunCmd,
        jsEnableEmail = enableEmail,
        jsEmailOverride = emailOverride,
        jsKeepNumEvals = keepNumEvals,
        jsLastCheckedTime = lastCheckedTime,
        jsLastEvalTime = lastEvalTime,
        jsErrorMsg = errorMsg,
        jsErrorTime = errorTime
      }

jobsetName :: Gen JobsetName
jobsetName = JobsetName <$> genTitle

jobsetId :: Gen JobsetId
jobsetId = JobsetId <$> Gen.int (Range.linear 0 maxBound)

jobsetState :: Gen JobsetState
jobsetState = Gen.element [JssEnabled, JssDisabled, JssOneShot, JssOneAtATime]

jobsetType :: Gen JobsetType
jobsetType = Gen.element [JstFlake, JstLegacy]

genTitle :: Gen Text
genTitle = Gen.text (Range.linear 0 255) Gen.unicode

genUrl :: Gen Text
genUrl = Gen.text len chars
  where
    -- maximum length of URL is essentially 2048
    len = Range.linear 0 2048
    -- latin1 should cover any URL-encoded string
    chars = Gen.latin1
