module Hydra.Pleco.Server.TestDb
  ( TestDb (..),
    lookupTestDb,
    withHydraDb,
  ) where

import Hydra.Pleco.Server.DB (withConnectionPool)

import Data.Unique (hashUnique, newUnique)
import Hasql.Pool (Pool)
import Hasql.Pool qualified as Pool
import Hasql.Session qualified as Session
import UnliftIO (bracket, throwIO)

data TestDb = TestDb
  { -- | libpq conninfo, in keyword/value form
    tdAdmin :: Text,
    -- | path to Hydra schema SQL
    tdSchema :: FilePath
  }
  deriving stock (Eq, Show)

seedPath :: FilePath
seedPath = "db-test/data/seed.sql"

lookupTestDb :: IO (Maybe TestDb)
lookupTestDb = do
  schema <- lookupEnv "PLECO_TEST_HYDRA_SCHEMA"
  admin <- fromMaybe "" <$> lookupEnv "PLECO_TEST_DATABASE_URL"
  pure $ TestDb (toText admin) <$> schema

-- | Create an ephemeral database, run the Hydra schema SQL, and run an action, then drop
-- the database
withHydraDb :: TestDb -> (Text -> Pool -> IO a) -> IO a
withHydraDb td act =
  withConnectionPool (tdAdmin td) $ \adminPool ->
    bracket (createDb adminPool) (dropDb adminPool) $ \name -> do
      let connInfo = tdAdmin td <> " dbname=" <> name
      withConnectionPool connInfo $ \pool -> do
        runSql pool =<< readFileBS (tdSchema td)
        runSql pool =<< readFileBS seedPath
        act connInfo pool

createDb :: Pool -> IO Text
createDb pool = do
  name <- ("pleco_test_" <>) . show . hashUnique <$> newUnique
  runSql pool (encodeUtf8 $ "drop database if exists " <> name)
  runSql pool (encodeUtf8 $ "create database " <> name)
  pure name

dropDb :: Pool -> Text -> IO ()
dropDb pool name = runSql pool (encodeUtf8 $ "drop database if exists " <> name)

runSql :: Pool -> ByteString -> IO ()
runSql pool = either throwIO pure <=< (Pool.use pool . Session.sql)
