module Main (main) where

import Control.Exception (throwIO)
import Hydra.Pleco.Client
  ( BaseUrl (..),
    Scheme (..),
    defaultManagerSettings,
    getHealth,
    mkPlecoClientEnv,
    newManager,
    runEchoServer,
    runPlecoClient,
  )
import Network.Wai.Handler.Warp (Port)
import Options.Applicative (Parser, ParserInfo)
import Options.Applicative qualified as Opt

data Options = Options
  { optCommand :: !Command,
    optVerbose :: !Bool
  }
  deriving stock (Show)

data Command
  = Health HealthOptions
  | Echo EchoOptions
  deriving stock (Eq, Ord, Show)

data HealthOptions = HealthOptions
  deriving stock (Eq, Ord, Show)

newtype EchoOptions = EchoOptions
  { optPort :: Port
  }
  deriving stock (Eq, Ord, Show)

main :: IO ()
main = Opt.execParser globalOptions >>= run

run :: Options -> IO ()
run Options {optCommand}
  | Health {} <- optCommand = runHealth
  | Echo EchoOptions {optPort} <- optCommand = runEcho optPort

runHealth :: IO ()
runHealth = do
  manager' <- newManager defaultManagerSettings
  let env = mkPlecoClientEnv manager' (BaseUrl Http "localhost" 8081 "")
  res <- runPlecoClient env getHealth
  either throwIO print res

runEcho :: Port -> IO ()
runEcho = runEchoServer

globalOptions :: ParserInfo Options
globalOptions =
  Opt.info (parser <**> Opt.helper) $
    Opt.fullDesc
      <> Opt.progDesc "Command line client for hydra-pleco API"
      <> Opt.header "hydra-pleco command-line client"

parser :: Parser Options
parser =
  Options
    <$> parseCommand
    <*> parseVerboseOpt

parseVerboseOpt :: Parser Bool
parseVerboseOpt =
  Opt.switch $
    Opt.long "verbose"
      <> Opt.short 'v'
      <> Opt.help "Verbose output?"

parseCommand :: Parser Command
parseCommand =
  Opt.hsubparser $
    Opt.command "health" parseHealthCmd
      <> Opt.command "echo" parseEchoCmd

parseHealthCmd :: ParserInfo Command
parseHealthCmd = Opt.info parseHealthOpt healthCmdInfo
  where
    parseHealthOpt = pure $ Health HealthOptions
    healthCmdInfo = Opt.progDesc "Check the server's health"

parseEchoCmd :: ParserInfo Command
parseEchoCmd = Opt.info (Echo <$> parseEchoOpt) echoCmdInfo
  where
    echoCmdInfo = Opt.progDesc "Receive jobset event callbacks and print them"

parseEchoOpt :: Parser EchoOptions
parseEchoOpt =
  EchoOptions
    <$> parsePortOpt

parsePortOpt :: Parser Port
parsePortOpt =
  Opt.option Opt.auto $
    Opt.long "port"
      <> Opt.short 'p'
      <> Opt.value 8888
      <> Opt.showDefault
      <> Opt.metavar "PORT"
      <> Opt.help "Port to listen on"
