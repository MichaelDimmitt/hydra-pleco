module Hydra.Pleco.Server.Monad
  ( PlecoServerT (..),
    PlecoServerEnv (..),
    mkPlecoServerEnv,
    runPlecoServerT,
  ) where

import Hydra.Pleco.Api (Subscription)
import Hydra.Pleco.Server.DB (newConnectionPool)

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Data.Text.Lazy.Builder (fromText)
import Hasql.Pool (Pool)
import Katip (ItemFormatter, Katip, KatipContext, LogContexts, LogEnv, LogItem, Namespace)
import Katip qualified
import Katip.Core qualified as Katip
import Katip.Format.Time qualified as Katip
import Katip.Scribes.Handle qualified as Katip
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import UnliftIO (MonadUnliftIO)

-- | The application monad stack
newtype PlecoServerT m a = PlecoServerT {unPlecoServerT :: ReaderT PlecoServerEnv m a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadCatch,
      MonadMask,
      MonadThrow,
      MonadReader PlecoServerEnv,
      MonadIO,
      MonadUnliftIO
    )

-- | The reader environment
data PlecoServerEnv = PlecoServerEnv
  { pseLogNamespace :: Namespace,
    pseLogCtx :: LogContexts,
    pseLogEnv :: LogEnv,
    pseSubscriptions :: TVar [Subscription],
    pseClientManager :: Manager,
    pseDbPool :: Pool
  }

instance (MonadIO io) => Katip (PlecoServerT io) where
  getLogEnv = asks pseLogEnv
  localLogEnv f (PlecoServerT m) =
    PlecoServerT $
      local
        (\env@PlecoServerEnv {..} -> env {pseLogEnv = f pseLogEnv})
        m

instance (MonadIO io) => KatipContext (PlecoServerT io) where
  getKatipContext = asks pseLogCtx

  localKatipContext f (PlecoServerT m) =
    PlecoServerT $
      local (\env@PlecoServerEnv {..} -> env {pseLogCtx = f pseLogCtx}) m

  getKatipNamespace = asks pseLogNamespace

  localKatipNamespace f (PlecoServerT m) =
    PlecoServerT $
      local (\env@PlecoServerEnv {..} -> env {pseLogNamespace = f pseLogNamespace}) m

runPlecoServerT :: PlecoServerEnv -> PlecoServerT m a -> m a
runPlecoServerT env = usingReaderT env . unPlecoServerT

mkPlecoServerEnv :: [Subscription] -> IO PlecoServerEnv
mkPlecoServerEnv initialSubs = do
  logEnv <- Katip.initLogEnv "hydra-pleco" "production"
  scribe <-
    Katip.mkHandleScribeWithFormatter
      logFormat
      Katip.ColorIfTerminal
      stderr
      (Katip.permitItem Katip.InfoS)
      Katip.V2
  logEnv' <- Katip.registerScribe "stderr" scribe Katip.defaultScribeSettings logEnv
  subs <- newTVarIO initialSubs
  pool <- newConnectionPool
  manager' <- newManager defaultManagerSettings

  pure
    PlecoServerEnv
      { pseLogNamespace = mempty,
        pseLogCtx = mempty,
        pseLogEnv = logEnv',
        pseSubscriptions = subs,
        pseDbPool = pool,
        pseClientManager = manager'
      }

-- | Katip's built-in 'bracketFormat' copied here, with some fields omitted. The
-- following fields have been removed:
--
--  * PID
--  * Thread ID
logFormat :: (LogItem a) => ItemFormatter a
logFormat withColor verb Katip.Item {..} =
  Katip.brackets nowStr
    <> Katip.brackets (mconcat $ map fromText $ Katip.intercalateNs _itemNamespace)
    <> Katip.brackets (fromText (renderSeverity' _itemSeverity))
    <> Katip.brackets (fromString _itemHost)
    <> mconcat ks
    <> maybe mempty (Katip.brackets . fromString . Katip.locationToString) _itemLoc
    <> fromText " "
    <> Katip.unLogStr _itemMessage
  where
    nowStr = fromText (Katip.formatAsLogTime _itemTime)
    ks = map Katip.brackets $ Katip.getKeys verb _itemPayload
    renderSeverity' severity =
      Katip.colorBySeverity withColor severity (Katip.renderSeverity severity)
