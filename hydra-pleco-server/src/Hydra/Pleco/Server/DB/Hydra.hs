module Hydra.Pleco.Server.DB.Hydra
  ( Project (..),
    Jobset (..),
    JobsetId (..),
    HydraNotification (..),
    projectSchema,
    jobsetSchema,
    jobsetById,
    hydraNotifyChannels,
    parseHydraNotification,
  ) where

import Rel8
  ( Column,
    DBEq,
    DBType,
    Name,
    Rel8able,
    Result,
    TableSchema,
    (==.),
  )
import Rel8 qualified
import Hasql.Statement (Statement)

data Project f = Project
  { prjName :: Column f Text,
    prjDisplayName :: Column f Text,
    prjDescription :: Column f Text,
    prjEnabled :: Column f Bool,
    prjHidden :: Column f Bool,
    prjOwner :: Column f Text,
    prjHomepage :: Column f Text,
    prjDeclFile :: Column f Text,
    prjDeclType :: Column f Text,
    prjEnableDynCmd :: Column f Bool
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (Project f)

data Jobset f = Jobset
  { jsName :: Column f Text,
    jsId :: Column f JobsetId,
    jsProject :: Column f Text,
    jsDescription :: Column f (Maybe Text),
    jsNixExprInput :: Column f (Maybe Text),
    jsNixExprPath :: Column f (Maybe Text),
    jsErrorMsg :: Column f (Maybe Text),
    jsErrorTime :: Column f (Maybe Int64),
    jsLastCheckedTime :: Column f (Maybe Int64),
    jsTriggerTime :: Column f (Maybe Int64),
    jsEnabled :: Column f Bool,
    jsEnableEmail :: Column f Text,
    jsHidden :: Column f Bool,
    jsCheckInterval :: Column f Int64,
    jsSchedulingShares :: Column f Int64,
    jsFetchErrorMsg :: Column f (Maybe Text),
    jsForceEval :: Column f (Maybe Bool),
    jsStartTime :: Column f (Maybe Int64),
    jsType :: Column f Int64,
    jsFlake :: Column f (Maybe Text),
    jsEnableDynCmd :: Column f Bool
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

projectSchema :: TableSchema (Project Name)
projectSchema =
  Rel8.TableSchema
    { name = "projects",
      columns = columnSchema
    }
  where
    columnSchema =
      Project
        { prjName = "name",
          prjDisplayName = "displayname",
          prjDescription = "description",
          prjEnabled = "enabled",
          prjHidden = "hidden",
          prjOwner = "owner",
          prjHomepage = "homepage",
          prjDeclFile = "declfile",
          prjDeclType = "decltype",
          prjEnableDynCmd = "enable_dynamic_run_command"
        }

jobsetSchema :: TableSchema (Jobset Name)
jobsetSchema = Rel8.TableSchema {name = "jobsets", columns = columnSchema}
  where
    columnSchema =
      Jobset
        { jsName = "name",
          jsId = "id",
          jsProject = "project",
          jsDescription = "description",
          jsNixExprInput = "nixexprinput",
          jsNixExprPath = "nixexprpath",
          jsErrorMsg = "errormsg",
          jsErrorTime = "errortime",
          jsLastCheckedTime = "lastcheckedtime",
          jsTriggerTime = "triggertime",
          jsEnabled = "enabled",
          jsEnableEmail = "enableemail",
          jsHidden = "hidden",
          jsCheckInterval = "checkinterval",
          jsSchedulingShares = "schedulingshares",
          jsFetchErrorMsg = "fetcherrormsg",
          jsForceEval = "forceeval",
          jsStartTime = "starttime",
          jsType = "type",
          jsFlake = "flake",
          jsEnableDynCmd = "enable_dynamic_run_command"
        }

data HydraNotification
  = HydraEvalAdded JobsetId JobsetEvalId
  | HydraEvalStarted JobsetId
  | HydraEvalCached JobsetId JobsetEvalId
  | HydraEvalFailed JobsetId
  deriving stock (Eq, Show)

newtype JobsetId = JobsetId {unJobsetId :: Int64}
  deriving newtype (DBEq, DBType, Eq, Read, Show)

newtype JobsetEvalId = JobsetEvalId {unJobsetEvalId :: Int}
  deriving stock (Eq, Show)
  deriving newtype (Read)

jobsetById :: JobsetId -> Statement () (Jobset Result)
jobsetById jobsetId = 
  Rel8.run1 $ 
    Rel8.select $ do
      jobsets <- Rel8.each jobsetSchema
      Rel8.where_ $ jsId jobsets ==. Rel8.lit jobsetId
      pure jobsets

hydraNotifyChannels :: [Text]
hydraNotifyChannels =
  [ "eval_added",
    "eval_started",
    "eval_cached",
    "eval_failed"
  ]

parseHydraNotification :: Text -> Text -> Either Text HydraNotification
parseHydraNotification channel payload =
  case (channel, payload') of
    ("eval_added", [_, jobsetId, evalId]) ->
      HydraEvalAdded <$> readEither' jobsetId <*> readEither' evalId
    ("eval_started", [_, jobsetId]) ->
      HydraEvalStarted <$> readEither' jobsetId
    ("eval_cached", [_, jobsetId, evalId]) ->
      HydraEvalCached <$> readEither' jobsetId <*> readEither' evalId
    ("eval_failed", [_, jobsetId]) ->
      HydraEvalFailed <$> readEither' jobsetId
    _ -> Left $ "Cannot parse event '" <> channel <> "' with payload: " <> payload
  where
    readEither' :: (Read a, ToString s) => s -> Either Text a
    readEither' = readEither . toString

    payload' = words payload
