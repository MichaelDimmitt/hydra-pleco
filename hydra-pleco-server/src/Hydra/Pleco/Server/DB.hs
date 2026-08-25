module Hydra.Pleco.Server.DB
  ( newConnectionPool,
    testConnection,
    releaseConnectionPool,
    runSession,
    statement,
    newConnection,
    releaseConnection,

    module Hydra
  ) where

import Hydra.Pleco.Server.DB.Hydra as Hydra

import Hasql.Connection.Setting (connection, Setting)
import Hasql.Connection.Setting.Connection (string)
import Hasql.Pool (Pool)
import Hasql.Pool qualified as Pool
import Hasql.Pool.Config qualified as Pool
import UnliftIO.Exception (throwIO)
import Hasql.Session (Session, statement)
import qualified Hasql.Connection as Connection
import Hasql.Connection (Connection)
import System.IO.Error (userError)

newConnectionPool :: MonadIO io => io Pool
newConnectionPool = liftIO $ Pool.acquire poolCfg
  where
    poolCfg =
      Pool.settings
        [ Pool.size 5,
          Pool.acquisitionTimeout 10,
          Pool.agingTimeout 86_400, -- One day
          Pool.idlenessTimeout 600, -- 10 minutes
          Pool.staticConnectionSettings [connectionString]
        ]

connectionString :: Setting
connectionString = connection (string "dbname=hydra")

testConnection :: MonadIO io => Pool -> io ()
testConnection pool = do
  res <- liftIO $ Pool.use pool pass
  either throwIO pure res

releaseConnectionPool :: MonadIO io => Pool -> io ()
releaseConnectionPool = liftIO . Pool.release

runSession :: MonadIO io => Pool -> Session a -> io a
runSession pool session = do
  res <- liftIO $ Pool.use pool session
  either throwIO pure res

newConnection :: MonadIO io => io Connection
newConnection = do
  res <- liftIO $ Connection.acquire [connectionString]
  either 
    (throwIO . userError . maybe "Could not acquire DB connection" decodeUtf8)
    pure
    res

releaseConnection :: MonadIO io => Connection -> io ()
releaseConnection = liftIO . Connection.release
