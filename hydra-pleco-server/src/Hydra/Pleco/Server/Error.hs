module Hydra.Pleco.Server.Error
  ( PlecoServerError (..),
  ) where
import Hasql.Connection (ConnectionError)
import Servant.Client (ClientError)

data PlecoServerError
  = ServerDbConnectionError ConnectionError
  | ServerClientError ClientError
  | ServerParsingError Text
  | ServerUnknownError Text
  deriving stock (Eq, Show)

instance Exception PlecoServerError
