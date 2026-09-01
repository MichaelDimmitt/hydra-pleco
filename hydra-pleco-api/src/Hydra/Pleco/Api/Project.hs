module Hydra.Pleco.Api.Project
  ( Project (..),
    ProjectId (..),
  ) where

import Data.Aeson (FromJSON, KeyValue (..), ToJSON, (.:))
import Data.Aeson qualified as Aeson
import Data.OpenApi (ToSchema, ToParamSchema)
import Servant.API (FromHttpApiData, ToHttpApiData)

data Project = Project
  { prjId :: ProjectId,
    prjEnabled :: Bool,
    prjVisible :: Bool,
    prjDisplayName :: Text,
    prjDescription :: Maybe Text,
    prjHomepage :: Maybe Text,
    prjOwner :: Text,
    prjEnableDynRunCmd :: Bool,
    prjDeclSpecFile :: Maybe FilePath,
    prjDeclInputType :: Maybe Text
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (ToSchema)

newtype ProjectId = ProjectId {unProjectId :: Text}
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (ToSchema)
  deriving newtype (ToJSON, FromJSON, FromHttpApiData, ToParamSchema, ToHttpApiData)

instance ToJSON Project where
  toJSON Project {..} =
    Aeson.object
      [ "id" .= prjId,
        "enabled" .= prjEnabled,
        "visible" .= prjVisible,
        "display_name" .= prjDisplayName,
        "description" .= prjDescription,
        "homepage" .= prjHomepage,
        "owner" .= prjOwner,
        "enable_dynamic_runcommand_hooks" .= prjEnableDynRunCmd,
        "declarative_spec_file" .= prjDeclSpecFile,
        "declarative_input_type" .= prjDeclInputType
      ]

  toEncoding Project {..} =
    Aeson.pairs $
      "id" .= prjId
        <> "enabled" .= prjEnabled
        <> "visible" .= prjVisible
        <> "display_name" .= prjDisplayName
        <> "description" .= prjDescription
        <> "homepage" .= prjHomepage
        <> "owner" .= prjOwner
        <> "enable_dynamic_runcommand_hooks" .= prjEnableDynRunCmd
        <> "declarative_spec_file" .= prjDeclSpecFile
        <> "declarative_input_type" .= prjDeclInputType

instance FromJSON Project where
  parseJSON = Aeson.withObject "Project" $ \val -> do
    name <- val .: "id"
    enabled <- val .: "enabled"
    visible <- val .: "visible"
    displayName <- val .: "display_name"
    description <- val .: "description"
    homepage <- val .: "homepage"
    owner <- val .: "owner"
    enableDynRunCmd <- val .: "enable_dynamic_runcommand_hooks"
    declSpecFile <- val .: "declarative_spec_file"
    declInputType <- val .: "declarative_input_type"

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
