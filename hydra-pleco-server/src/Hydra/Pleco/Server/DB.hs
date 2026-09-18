module Hydra.Pleco.Server.DB
  ( newConnectionPool,
    withConnectionPool,
    testConnection,
    releaseConnectionPool,
    runSession,
    statement,
    newConnection,
    releaseConnection,
    module Hydra,
  ) where

import Hydra.Pleco.Server.DB.Hydra as Hydra
import Hydra.Pleco.Server.Error (PlecoServerError (..))

import Hasql.Connection (Connection)
import Hasql.Connection qualified as Connection
import Hasql.Connection.Setting (Setting, connection)
import Hasql.Connection.Setting.Connection (string)
import Hasql.Pool (Pool)
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as Pool
import Hasql.Session (Session, statement)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket, throwIO)

newConnectionPool :: (MonadIO io) => Text -> io Pool
newConnectionPool connInfo = liftIO $ Pool.acquire poolCfg
  where
    poolCfg =
      Pool.settings
        [ Pool.size 5,
          Pool.acquisitionTimeout 10,
          Pool.agingTimeout 86_400, -- One day
          Pool.idlenessTimeout 600, -- 10 minutes
          Pool.staticConnectionSettings [connectionSetting connInfo]
        ]

withConnectionPool :: (MonadUnliftIO io) => Text -> (Pool -> io a) -> io a
withConnectionPool connInfo = bracket (newConnectionPool connInfo) releaseConnectionPool

-- | libpq reads an empty conninfo from the PG* environment
connectionSetting :: Text -> Setting
connectionSetting = connection . string

testConnection :: (MonadIO io) => Pool -> io ()
testConnection pool = do
  res <- liftIO $ Pool.use pool pass
  either throwIO pure res

releaseConnectionPool :: (MonadIO io) => Pool -> io ()
releaseConnectionPool = liftIO . Pool.release

runSession :: (MonadIO io) => Pool -> Session a -> io a
runSession pool session = do
  res <- liftIO $ Pool.use pool session
  either throwIO pure res

newConnection :: (MonadIO io) => Text -> io Connection
newConnection connInfo = do
  res <- liftIO $ Connection.acquire [connectionSetting connInfo]
  either
    (throwIO . ServerDbConnectionError)
    pure
    res

releaseConnection :: (MonadIO io) => Connection -> io ()
releaseConnection = liftIO . Connection.release
