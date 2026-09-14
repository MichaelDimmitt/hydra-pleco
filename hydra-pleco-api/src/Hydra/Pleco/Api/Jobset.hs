module Hydra.Pleco.Api.Jobset
  ( Jobset (..),
    JobsetId (..),
    JobsetName (..),
    JobsetState (..),
    JobsetType (..),
  ) where

import Hydra.Pleco.Api.Project (ProjectId)

import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON, (.:))
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict.InsOrd.Compat qualified as InsOrd
import Data.OpenApi (ToParamSchema, ToSchema (..))
import Data.OpenApi qualified as OpenApi
import Optics ((.~), (?~))
import Servant.API (FromHttpApiData, ToHttpApiData)

data Jobset = Jobset
  { jsName :: JobsetName,
    jsProject :: ProjectId,
    jsId :: JobsetId,
    jsState :: JobsetState,
    jsVisible :: Bool,
    jsType :: JobsetType,
    jsFlake :: Maybe Text,
    jsNixExprInput :: Maybe Text,
    jsNixExprPath :: Maybe Text,
    jsDescription :: Maybe Text,
    jsCheckInterval :: Int,
    jsSchedulingShares :: Int,
    jsEnableDynRunCmd :: Bool,
    jsEnableEmail :: Bool,
    jsEmailOverride :: Maybe Text,
    jsKeepNumEvals :: Int,
    jsLastCheckedTime :: Maybe Int,
    jsLastEvalTime :: Maybe Int,
    jsErrorMsg :: Maybe Text,
    jsErrorTime :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)

newtype JobsetId = JobsetId {unJobsetId :: Int}
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (ToJSON, FromJSON, FromHttpApiData, ToParamSchema, ToHttpApiData, ToSchema)

newtype JobsetName = JobsetName {unJobsetName :: Text}
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (ToJSON, FromJSON, FromHttpApiData, ToParamSchema, ToSchema, ToHttpApiData)

data JobsetState
  = JssEnabled
  | JssDisabled
  | JssOneShot
  | JssOneAtATime
  deriving stock (Bounded, Eq, Enum, Generic, Ord, Show)

data JobsetType
  = JstFlake
  | JstLegacy
  deriving stock (Bounded, Eq, Enum, Generic, Ord, Show)

instance ToJSON Jobset where
  toJSON Jobset {..} =
    Aeson.object
      [ "name" .= jsName,
        "project" .= jsProject,
        "id" .= jsId,
        "state" .= jsState,
        "visible" .= jsVisible,
        "type" .= jsType,
        "flake" .= jsFlake,
        "nix_expr_input" .= jsNixExprInput,
        "nix_expr_path" .= jsNixExprPath,
        "description" .= jsDescription,
        "check_interval" .= jsCheckInterval,
        "scheduling_shares" .= jsSchedulingShares,
        "enable_dynamic_runcommand_hooks" .= jsEnableDynRunCmd,
        "enable_email_notification" .= jsEnableEmail,
        "email_override" .= jsEmailOverride,
        "keep_number_evaluations" .= jsKeepNumEvals,
        "last_checked_time" .= jsLastCheckedTime,
        "last_eval_time" .= jsLastEvalTime,
        "error_msg" .= jsErrorMsg,
        "error_time" .= jsErrorTime
      ]

  toEncoding Jobset {..} =
    Aeson.pairs $
      "name" .= jsName
        <> "project" .= jsProject
        <> "id" .= jsId
        <> "state" .= jsState
        <> "visible" .= jsVisible
        <> "type" .= jsType
        <> "flake" .= jsFlake
        <> "nix_expr_input" .= jsNixExprInput
        <> "nix_expr_path" .= jsNixExprPath
        <> "description" .= jsDescription
        <> "check_interval" .= jsCheckInterval
        <> "scheduling_shares" .= jsSchedulingShares
        <> "enable_dynamic_runcommand_hooks" .= jsEnableDynRunCmd
        <> "enable_email_notification" .= jsEnableEmail
        <> "email_override" .= jsEmailOverride
        <> "keep_number_evaluations" .= jsKeepNumEvals
        <> "last_checked_time" .= jsLastCheckedTime
        <> "last_eval_time" .= jsLastEvalTime
        <> "error_msg" .= jsErrorMsg
        <> "error_time" .= jsErrorTime

instance FromJSON Jobset where
  parseJSON = Aeson.withObject "Jobset" $ \val -> do
    name <- val .: "name"
    project <- val .: "project"
    id' <- val .: "id"
    state' <- val .: "state"
    visible <- val .: "visible"
    jsType <- val .: "type"
    flake <- val .: "flake"
    nixExprInput <- val .: "nix_expr_input"
    nixExprPath <- val .: "nix_expr_path"
    description <- val .: "description"
    checkInterval <- val .: "check_interval"
    schedulingShares <- val .: "scheduling_shares"
    enableDynRunCmd <- val .: "enable_dynamic_runcommand_hooks"
    enableEmail <- val .: "enable_email_notification"
    emailOverride <- val .: "email_override"
    keepNumEvals <- val .: "keep_number_evaluations"
    lastCheckedTime <- val .: "last_checked_time"
    lastEvalTime <- val .: "last_eval_time"
    errorMsg <- val .: "error_msg"
    errorTime <- val .: "error_time"

    pure
      Jobset
        { jsName = name,
          jsProject = project,
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

instance ToSchema Jobset where
  declareNamedSchema _ = do
    stateSchema <- OpenApi.declareSchemaRef (Proxy @JobsetState)
    typeSchema <- OpenApi.declareSchemaRef (Proxy @JobsetType)

    let properties =
          InsOrd.fromList
            [ ("name", OpenApi.toSchemaRef (Proxy @JobsetName)),
              ("project", OpenApi.toSchemaRef (Proxy @ProjectId)),
              ("id", OpenApi.toSchemaRef (Proxy @JobsetId)),
              ("state", stateSchema),
              ("visible", OpenApi.toSchemaRef (Proxy @Bool)),
              ("type", typeSchema),
              ("flake", OpenApi.toSchemaRef (Proxy @Text)),
              ("nix_expr_input", OpenApi.toSchemaRef (Proxy @(Maybe Text))),
              ("nix_expr_path", OpenApi.toSchemaRef (Proxy @(Maybe Text))),
              ("description", OpenApi.toSchemaRef (Proxy @(Maybe Text))),
              ("check_interval", OpenApi.toSchemaRef (Proxy @Int)),
              ("scheduling_shares", OpenApi.toSchemaRef (Proxy @Int)),
              ("enable_dynamic_runcommand_hooks", OpenApi.toSchemaRef (Proxy @Bool)),
              ("enable_email_notification", OpenApi.toSchemaRef (Proxy @Bool)),
              ("email_override", OpenApi.toSchemaRef (Proxy @(Maybe Text))),
              ("keep_number_evaluations", OpenApi.toSchemaRef (Proxy @Int)),
              ("last_checked_time", OpenApi.toSchemaRef (Proxy @(Maybe Int))),
              ("last_eval_time", OpenApi.toSchemaRef (Proxy @(Maybe Int))),
              ("error_msg", OpenApi.toSchemaRef (Proxy @(Maybe Text))),
              ("error_time", OpenApi.toSchemaRef (Proxy @(Maybe Int)))
            ]

        required =
          [ "name",
            "project",
            "id",
            "state",
            "visible",
            "type",
            "check_interval",
            "scheduling_shares",
            "enable_dynamic_runcommand_hooks",
            "enable_email_notification",
            "keep_number_evaluations"
          ]

    pure $
      OpenApi.NamedSchema (Just "Jobset") $
        mempty
          & #properties .~ properties
          & #required .~ required
          & #type ?~ OpenApi.OpenApiObject

instance ToJSON JobsetState where
  toJSON JssEnabled = "ENABLED"
  toJSON JssDisabled = "DISABLED"
  toJSON JssOneShot = "ONE_SHOT"
  toJSON JssOneAtATime = "ONE_AT_A_TIME"

instance FromJSON JobsetState where
  parseJSON = Aeson.withText "JobsetState" $ \case
    "ENABLED" -> pure JssEnabled
    "DISABLED" -> pure JssDisabled
    "ONE_SHOT" -> pure JssOneShot
    "ONE_AT_A_TIME" -> pure JssOneAtATime
    st -> fail (toString st)

instance ToSchema JobsetState where
  declareNamedSchema _ =
    pure $
      OpenApi.NamedSchema (Just "JobsetState") $
        mempty
          & #enum ?~ map Aeson.toJSON (universe @JobsetState)
          & #type ?~ OpenApi.OpenApiString

instance ToJSON JobsetType where
  toJSON JstFlake = "FLAKE"
  toJSON JstLegacy = "LEGACY"

instance FromJSON JobsetType where
  parseJSON = Aeson.withText "JobsetType" $ \case
    "FLAKE" -> pure JstFlake
    "LEGACY" -> pure JstLegacy
    ty -> fail (toString ty)

instance ToSchema JobsetType where
  declareNamedSchema _ =
    pure $
      OpenApi.NamedSchema (Just "JobsetType") $
        mempty
          & #enum ?~ map Aeson.toJSON (universe @JobsetType)
          & #type ?~ OpenApi.OpenApiString
