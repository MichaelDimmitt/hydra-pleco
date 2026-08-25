module Hydra.Pleco.Server.DB.Hydra
  ( Project (..),
    projectSchema,
    HydraNotification (..),
    hydraNotifyChannels,
    parseHydraNotification,
  ) where

import Rel8 (Column, Name, Rel8able, Result, TableSchema (..))

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

projectSchema :: TableSchema (Project Name)
projectSchema =
  TableSchema
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

data HydraNotification
  = HydraEvalAdded JobsetId JobsetEvalId
  | HydraEvalStarted JobsetId
  | HydraEvalCached JobsetId JobsetEvalId
  | HydraEvalFailed JobsetId
  deriving stock (Eq, Show)

newtype JobsetId = JobsetId {unJobsetId :: Int}
  deriving stock (Eq, Show)
  deriving newtype (Read)

newtype JobsetEvalId = JobsetEvalId {unJobsetEvalId :: Int}
  deriving stock (Eq, Show)
  deriving newtype (Read)

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
