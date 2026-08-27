module Main (main) where

import Hydra.Pleco.Api (Subscription (..))
import Hydra.Pleco.Server (mkPlecoServerEnv, runServer)

import Network.Wai.Handler.Warp (Port)
import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as Opt

data Options = Options
  { optPort :: !Port,
    optWebhooks :: ![Subscription]
  }
  deriving stock (Eq, Show)

main :: IO ()
main = Opt.execParser optionsInfo >>= run

run :: Options -> IO ()
run Options {optPort, optWebhooks} = do
  env <- mkPlecoServerEnv optWebhooks
  runServer optPort env

optionsInfo :: ParserInfo Options
optionsInfo =
  Opt.info (parseOptions <**> Opt.helper) $
    Opt.fullDesc
      <> Opt.progDesc "REST API and webhook server for hydra-pleco"
      <> Opt.header "hydra-pleco API server"

parseOptions :: Parser Options
parseOptions =
  Options
    <$> parsePort
    <*> parseWebhook

parsePort :: Parser Port
parsePort =
  Opt.option Opt.auto $
    Opt.long "port"
      <> Opt.short 'p'
      <> Opt.value 8081
      <> Opt.showDefault
      <> Opt.metavar "PORT"
      <> Opt.help "Port to listen on"

parseWebhook :: Parser [Subscription]
parseWebhook =
  many . Opt.option (Subscription <$> Opt.str) $
    Opt.long "webhook"
      <> Opt.short 'w'
      <> Opt.metavar "URL"
      <> Opt.help "Preseed /webhooks with a subscription to URL (repeatable)"
