module Main (main) where

import Hydra.Pleco.Client
  ( BaseUrl (..),
    PlecoClient,
    PlecoClientEnv,
    ProjectId (..),
    Scheme (..),
    ToJSON,
    encodePretty,
    getHealth,
    getProject,
    listProjects,
    mkPlecoClientEnv,
    parseBaseUrl,
    runEchoServer,
    runPlecoClient,
  )

import Network.Wai.Handler.Warp (Port)
import Options.Applicative (Parser, ParserInfo, ReadM)
import Options.Applicative qualified as Opt

data GlobalOpts = GlobalOpts
  { optUrl :: !BaseUrl,
    optVerbose :: !Bool,
    optCommand :: !Command
  }
  deriving stock (Show)

data Command
  = CmdHealth
  | CmdEcho EchoCmdOpts
  | CmdProjects ProjectsSubCommand
  deriving stock (Eq, Ord, Show)

newtype EchoCmdOpts = EchoCmdOpts
  { echoOptPort :: Port
  }
  deriving stock (Eq, Ord, Show)

data ProjectsSubCommand
  = CmdProjectsList
  | CmdProjectsView ProjectId
  deriving stock (Eq, Ord, Show)

main :: IO ()
main = Opt.execParser globalOpts >>= run

-- | Runs the 'PlecoClient' action
runClient :: PlecoClient a -> GlobalOpts -> IO a
runClient action opts = flip runPlecoClient action =<< fromGlobalOpts opts
  where
    fromGlobalOpts :: GlobalOpts -> IO PlecoClientEnv
    fromGlobalOpts GlobalOpts {optUrl} = mkPlecoClientEnv optUrl

-- | Same as @runClient@, but pretty prints the result as JSON
runClient' :: (ToJSON json) => PlecoClient json -> GlobalOpts -> IO ()
runClient' = (prettyPrintLBS <=<) . runClient
  where
    prettyPrintLBS :: (ToJSON json) => json -> IO ()
    prettyPrintLBS = putLBSLn . encodePretty

run :: GlobalOpts -> IO ()
run opts@GlobalOpts {optCommand}
  | CmdHealth <- optCommand = runHealth opts
  | CmdEcho EchoCmdOpts {echoOptPort} <- optCommand = runEcho echoOptPort
  | CmdProjects subCmd <- optCommand = runProjects subCmd opts

runHealth :: GlobalOpts -> IO ()
runHealth = runClient' getHealth

runEcho :: Port -> IO ()
runEcho = runEchoServer

runProjects :: ProjectsSubCommand -> GlobalOpts -> IO ()
runProjects CmdProjectsList = runClient' listProjects
runProjects (CmdProjectsView p) = runClient' (getProject p)

globalOpts :: ParserInfo GlobalOpts
globalOpts =
  Opt.info (parser <**> Opt.helper) $
    Opt.fullDesc
      <> Opt.progDesc "Command line client for hydra-pleco API"
      <> Opt.header "hydra-pleco command-line client"

parser :: Parser GlobalOpts
parser = do
  GlobalOpts
    <$> parseUrlOpt
    <*> parseVerboseOpt
    <*> parseCommand

parseUrlOpt :: Parser BaseUrl
parseUrlOpt =
  Opt.option readBaseUrl $
    Opt.long "url"
      <> Opt.short 'u'
      <> Opt.value (BaseUrl Http "localhost" 8081 "")
      <> Opt.help "Pleco server endpoint URL"
  where
    readBaseUrl :: ReadM BaseUrl
    readBaseUrl = Opt.eitherReader $ first displayException . parseBaseUrl

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
      <> Opt.command "projects" projectsOpts

parseHealthCmd :: ParserInfo Command
parseHealthCmd = Opt.info parseHealthOpt healthCmdInfo
  where
    parseHealthOpt = pure CmdHealth
    healthCmdInfo = Opt.progDesc "Check the server's health"

parseEchoCmd :: ParserInfo Command
parseEchoCmd = Opt.info (CmdEcho <$> parseEchoOpt) echoCmdInfo
  where
    echoCmdInfo = Opt.progDesc "Receive jobset event callbacks and print them"

parseEchoOpt :: Parser EchoCmdOpts
parseEchoOpt = EchoCmdOpts <$> parseEchoPortOpt

parseEchoPortOpt :: Parser Port
parseEchoPortOpt =
  Opt.option Opt.auto $
    Opt.long "port"
      <> Opt.short 'p'
      <> Opt.value 8888
      <> Opt.showDefault
      <> Opt.metavar "PORT"
      <> Opt.help "Port to listen on"

projectsOpts :: ParserInfo Command
projectsOpts = Opt.info (CmdProjects <$> parseProjectsCmd) projectsCmdInfo
  where
    projectsCmdInfo = Opt.progDesc "List or view Hydra projects"

parseProjectsCmd :: Parser ProjectsSubCommand
parseProjectsCmd =
  Opt.hsubparser $
    Opt.command "list" parseProjectsListCmd
      <> Opt.command "view" parseProjectsViewCmd

parseProjectsListCmd :: ParserInfo ProjectsSubCommand
parseProjectsListCmd = Opt.info (pure CmdProjectsList) projectsListCmdInfo
  where
    projectsListCmdInfo = Opt.progDesc "List all Hydra projects"

parseProjectsViewCmd :: ParserInfo ProjectsSubCommand
parseProjectsViewCmd = Opt.info parseCmd cmdInfo
  where
    cmdInfo = Opt.progDesc "View a Hydra project"
    parseCmd = CmdProjectsView <$> parseProjectId

parseProjectId :: Parser ProjectId
parseProjectId =
  Opt.argument (ProjectId <$> Opt.str) $
    Opt.metavar "NAME"
      <> Opt.help "Hydra project identifier"
