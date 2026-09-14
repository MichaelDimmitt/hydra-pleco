module Hydra.Pleco.Api.Project
  ( Project (..),
    ProjectId (..),
  ) where

import Data.Aeson (FromJSON, KeyValue (..), ToJSON, (.:))
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict.InsOrd.Compat qualified as InsOrd
import Data.OpenApi (ToParamSchema, ToSchema (..))
import Data.OpenApi qualified as OpenApi
import Optics ((.~), (?~))
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

newtype ProjectId = ProjectId {unProjectId :: Text}
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (ToJSON, FromJSON, FromHttpApiData, ToParamSchema, ToHttpApiData, ToSchema)

instance ToSchema Project where
  declareNamedSchema _ = do
    let properties =
          InsOrd.fromList
            [ ("id", OpenApi.toSchemaRef (Proxy @ProjectId)),
              ("enabled", OpenApi.toSchemaRef (Proxy @Bool)),
              ("visible", OpenApi.toSchemaRef (Proxy @Bool)),
              ("display_name", OpenApi.toSchemaRef (Proxy @Text)),
              ("description", OpenApi.toSchemaRef (Proxy @(Maybe Text))),
              ("homepage", OpenApi.toSchemaRef (Proxy @(Maybe Text))),
              ("owner", OpenApi.toSchemaRef (Proxy @Text)),
              ("enable_dynamic_runcommand_hooks", OpenApi.toSchemaRef (Proxy @Bool)),
              ("declarative_spec_file", OpenApi.toSchemaRef (Proxy @(Maybe FilePath))),
              ("declarative_input_type", OpenApi.toSchemaRef (Proxy @(Maybe Text)))
            ]

        required =
          [ "id",
            "enabled",
            "visible",
            "display_name",
            "owner",
            "enable_dynamic_runcommand_hooks"
          ]

    pure $
      OpenApi.NamedSchema (Just "Project") $
        mempty
          & #properties .~ properties
          & #required .~ required
          & #type ?~ OpenApi.OpenApiObject

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
