module Main (main) where

import Hydra.Pleco.Server (mkPlecoServerEnv, runServer)

main :: IO ()
main = do
  env <- mkPlecoServerEnv
  runServer 8081 env
