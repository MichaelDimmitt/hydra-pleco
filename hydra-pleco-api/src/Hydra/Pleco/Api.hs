module Hydra.Pleco.Api
  ( HydraApi (..),
    WebhooksApi (..),
    HydraApp (..),
    HealthJSON,
    Health (..),
    Subscription (..),
    JobsetEventApi (..),
    JobsetEvent (..),
    EventType (..),
    ProjectsApi (..),
    Project (..),
    ProjectId (..),
    JobsetsApi (..),
    Jobset (..),
    JobsetId (..),
    JobsetName (..),
    JobsetState (..),
    JobsetType (..),
    EvalsApi (..),
    Eval (..),
    EvalId (..),
    hydraApi,
    hydraOpenApi,
    jobsetEventApi,
  ) where

import Hydra.Pleco.Api.Eval (Eval (..), EvalId (..))
import Hydra.Pleco.Api.Event
  ( EventType (..),
    JobsetEvent (..),
    JobsetEventApi (..),
    jobsetEventApi,
  )
import Hydra.Pleco.Api.Jobset
  ( Jobset (..),
    JobsetId (..),
    JobsetName (..),
    JobsetState (..),
    JobsetType (..),
  )
import Hydra.Pleco.Api.Project (Project (..), ProjectId (..))

import Data.Aeson
  ( FromJSON,
    KeyValue (..),
    ToJSON,
    (.:),
  )
import Data.Aeson qualified as Aeson
import Data.HashMap.Strict.InsOrd.Compat qualified as InsOrd
import Data.OpenApi
  ( Callback,
    Definitions,
    OpenApi,
    Operation,
    Referenced,
    RequestBody,
    Schema,
    ToSchema,
  )
import Data.OpenApi qualified as OpenApi
import Data.OpenApi.Declare (execDeclare)
import Network.HTTP.Media ((//))
import Optics (At (..), (%), (%~), (.~), (?~))
import Optics.Extra (_Just)
import Servant.API
import Servant.OpenApi (HasOpenApi (..))
import Servant.Swagger.UI (SwaggerSchemaUI)

-- | Documented API routes. OpenApi spec is generated from this.
data HydraApi mode = HydraApi
  { apiHealth :: mode :- "health" :> Get '[HealthJSON] Health,
    apiWebhooks :: mode :- "webhooks" :> NamedRoutes WebhooksApi,
    apiProjects :: mode :- "projects" :> NamedRoutes ProjectsApi,
    apiJobsets :: mode :- "jobsets" :> NamedRoutes JobsetsApi,
    apiEvals :: mode :- "evals" :> NamedRoutes EvalsApi
  }
  deriving stock (Generic)

data WebhooksApi mode = WebhooksApi
  { whaSubscribe :: mode :- ReqBody '[JSON] Subscription :> PostCreated '[JSON] Subscription,
    whaList :: mode :- Get '[JSON] [Subscription]
  }
  deriving stock (Generic)

data ProjectsApi mode = ProjectsApi
  { prjaList :: mode :- Get '[JSON] [Project],
    prjaGet :: mode :- Capture "id" ProjectId :> Get '[JSON] Project,
    prjaJobsets :: mode :- Capture "id" ProjectId :> "jobsets" :> Get '[JSON] [Jobset]
  }
  deriving stock (Generic)

data JobsetsApi mode = JobsetsApi
  { jsaGet :: mode :- Capture "id" JobsetId :> Get '[JSON] Jobset,
    jsaEvals :: mode :- Capture "id" JobsetId :> "evals" :> Get '[JSON] [Eval]
  }
  deriving stock (Generic)

newtype EvalsApi mode = EvalsApi
  { evaGet :: mode :- Capture "id" EvalId :> Get '[JSON] Eval
  }
  deriving stock (Generic)

-- | The full Rest API application: the documented routes plus the Swagger UI.
data HydraApp mode = HydraApp
  { appApi :: mode :- NamedRoutes HydraApi,
    appDocs :: mode :- SwaggerSchemaUI "swagger-ui" "swagger.json"
  }
  deriving stock (Generic)

newtype Health = Health
  {hlStatus :: Text}
  deriving stock (Eq, Show, Generic)

data HealthJSON

newtype Subscription = Subscription
  {subUrl :: Text}
  deriving stock (Eq, Show, Generic)

instance ToSchema Health where
  declareNamedSchema _ = do
    let properties = InsOrd.fromList [("status", OpenApi.toSchemaRef @Text Proxy)]

    pure $
      OpenApi.NamedSchema (Just "Health") $
        mempty
          & #properties .~ properties
          & #required .~ ["status"]
          & #type ?~ OpenApi.OpenApiObject

instance ToJSON Health where
  toJSON Health {hlStatus} =
    Aeson.object
      [ "status" .= hlStatus
      ]

  toEncoding Health {hlStatus} =
    Aeson.pairs $
      "status" .= hlStatus

instance FromJSON Health where
  parseJSON = Aeson.withObject "Health" $ \val ->
    Health
      <$> val .: "status"

instance Accept HealthJSON where
  contentType _ = "application" // "health+json"

instance (ToJSON json) => MimeRender HealthJSON json where
  mimeRender _ = Aeson.encode

instance (FromJSON json) => MimeUnrender HealthJSON json where
  mimeUnrender _ = Aeson.eitherDecode

instance ToSchema Subscription where
  declareNamedSchema _ = do
    let properties = InsOrd.fromList [("url", OpenApi.toSchemaRef @Text Proxy)]

    pure $
      OpenApi.NamedSchema (Just "Subscription") $
        mempty
          & #properties .~ properties
          & #required .~ ["url"]
          & #type ?~ OpenApi.OpenApiObject

instance ToJSON Subscription where
  toJSON Subscription {subUrl} = Aeson.object ["url" .= subUrl]
  toEncoding Subscription {subUrl} = Aeson.pairs $ "url" .= subUrl

instance FromJSON Subscription where
  parseJSON = Aeson.withObject "Subscription" $ \val ->
    Subscription <$> val .: "url"

hydraApi :: Proxy HydraApi
hydraApi = Proxy

hydraOpenApi :: OpenApi
hydraOpenApi =
  toOpenApi (Proxy :: Proxy (NamedRoutes HydraApi))
    & #components % #schemas %~ (<> mkEventSchema)
    & #components % #callbacks % at "jobsetEvent" ?~ mkEventCallback
    & pathCallbacks "/webhooks" % at "jobsetEvent" ?~ mkCallbackEventRef
  where
    pathCallbacks path = #paths % at path % _Just % #post % _Just % #callbacks

    mkEventSchema :: Definitions Schema
    mkEventSchema =
      execDeclare (OpenApi.declareSchemaRef (Proxy @JobsetEvent)) mempty

    mkEventCallback :: Callback
    mkEventCallback =
      OpenApi.Callback $
        InsOrd.fromList [("{$request.body#/url}", mempty & #post ?~ mkEventOp)]

    mkEventOp :: Operation
    mkEventOp =
      mempty
        & #summary ?~ "Hydra Jobset event notification"
        & #description ?~ "Posted to each registered url on Hydra jobset events."
        & #requestBody ?~ OpenApi.Inline mkReqBody
        & #responses % at 200 ?~ OpenApi.Inline mempty

    mkReqBody :: RequestBody
    mkReqBody =
      mempty
        & #required ?~ True
        & #content % at "application/json" ?~ mkMediaType

    mkMediaType = mempty & #schema ?~ OpenApi.Ref (OpenApi.Reference "JobsetEvent")

    mkCallbackEventRef :: Referenced Callback
    mkCallbackEventRef = OpenApi.Ref (OpenApi.Reference "jobsetEvent")
