module Hydra.Pleco.Api.Eval
  ( Eval (..),
    EvalId (..),
  ) where

import Hydra.Pleco.Api.Jobset (JobsetId)

import Data.Aeson (FromJSON, KeyValue ((.=)), ToJSON, (.:))
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict.InsOrd.Compat qualified as InsOrd
import Data.OpenApi (ToParamSchema, ToSchema (..))
import Data.OpenApi qualified as OpenApi
import Optics ((.~), (?~))
import Servant.API (FromHttpApiData, ToHttpApiData)

data Eval = Eval
  { evId :: EvalId,
    evJobsetId :: JobsetId,
    evTimestamp :: Int,
    evCheckoutTime :: Int,
    evEvalTime :: Int,
    evHasNewBuilds :: Bool,
    evHash :: Text,
    evNumBuilds :: Maybe Int,
    evNumSucceeded :: Maybe Int,
    evFlake :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

newtype EvalId = EvalId {unEvalId :: Int}
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (ToJSON, FromJSON, FromHttpApiData, ToParamSchema, ToHttpApiData, ToSchema)

instance ToJSON Eval where
  toJSON Eval {..} =
    Aeson.object
      [ "id" .= evId,
        "jobset_id" .= evJobsetId,
        "timestamp" .= evTimestamp,
        "checkout_time" .= evCheckoutTime,
        "eval_time" .= evEvalTime,
        "has_new_builds" .= evHasNewBuilds,
        "hash" .= evHash,
        "num_builds" .= evNumBuilds,
        "num_succeeded" .= evNumSucceeded,
        "flake" .= evFlake
      ]

  toEncoding Eval {..} =
    Aeson.pairs $
      "id" .= evId
        <> "jobset_id" .= evJobsetId
        <> "timestamp" .= evTimestamp
        <> "checkout_time" .= evCheckoutTime
        <> "eval_time" .= evEvalTime
        <> "has_new_builds" .= evHasNewBuilds
        <> "hash" .= evHash
        <> "num_builds" .= evNumBuilds
        <> "num_succeeded" .= evNumSucceeded
        <> "flake" .= evFlake

instance FromJSON Eval where
  parseJSON = Aeson.withObject "Eval" $ \val -> do
    id' <- val .: "id"
    jobsetId <- val .: "jobset_id"
    timestamp <- val .: "timestamp"
    checkoutTime <- val .: "checkout_time"
    evalTime <- val .: "eval_time"
    hasNewBuilds <- val .: "has_new_builds"
    hash' <- val .: "hash"
    numBuilds <- val .: "num_builds"
    numSucceeded <- val .: "num_succeeded"
    flake <- val .: "flake"

    pure
      Eval
        { evId = id',
          evJobsetId = jobsetId,
          evTimestamp = timestamp,
          evCheckoutTime = checkoutTime,
          evEvalTime = evalTime,
          evHasNewBuilds = hasNewBuilds,
          evHash = hash',
          evNumBuilds = numBuilds,
          evNumSucceeded = numSucceeded,
          evFlake = flake
        }

instance ToSchema Eval where
  declareNamedSchema _ = do
    let properties =
          InsOrd.fromList
            [ ("id", OpenApi.toSchemaRef (Proxy @EvalId)),
              ("jobset_id", OpenApi.toSchemaRef (Proxy @JobsetId)),
              ("timestamp", OpenApi.toSchemaRef (Proxy @Int)),
              ("checkout_time", OpenApi.toSchemaRef (Proxy @Int)),
              ("eval_time", OpenApi.toSchemaRef (Proxy @Int)),
              ("has_new_builds", OpenApi.toSchemaRef (Proxy @Bool)),
              ("hash", OpenApi.toSchemaRef (Proxy @Text)),
              ("num_builds", OpenApi.toSchemaRef (Proxy @(Maybe Int))),
              ("num_succeeded", OpenApi.toSchemaRef (Proxy @(Maybe Int))),
              ("flake", OpenApi.toSchemaRef (Proxy @(Maybe Text)))
            ]

        required =
          [ "id",
            "jobset_id",
            "timestamp",
            "checkout_time",
            "eval_time",
            "has_new_builds",
            "hash"
          ]

    pure $
      OpenApi.NamedSchema (Just "Eval") $
        mempty
          & #properties .~ properties
          & #required .~ required
          & #type ?~ OpenApi.OpenApiObject
