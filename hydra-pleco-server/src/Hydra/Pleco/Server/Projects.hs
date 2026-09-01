module Hydra.Pleco.Server.Projects
  ( projectsHandler,
  ) where

import Hydra.Pleco.Api (Project (..), ProjectId (..), ProjectsApi (..))
import Hydra.Pleco.Server.Monad (PlecoServerT)

import Data.List qualified as List
import Servant (Handler, HasServer (..), NamedRoutes)

projectsHandler :: ServerT (NamedRoutes ProjectsApi) (PlecoServerT Handler)
projectsHandler =
  ProjectsApi
    { listProjects = listProjectsHandler,
      getProject = getProjectHandler
    }

-- TODO[sgillespie]: Implement me
listProjectsHandler :: PlecoServerT Handler [Project]
listProjectsHandler = List.singleton <$> getProjectHandler (ProjectId "demo-project")

-- TODO[sgillespie]: Implement me
getProjectHandler :: ProjectId -> PlecoServerT Handler Project
getProjectHandler _ =
  pure
    Project
      { prjId = ProjectId "demo-project",
        prjEnabled = True,
        prjVisible = True,
        prjDisplayName = "Demo Project",
        prjDescription = Nothing,
        prjHomepage = Nothing,
        prjOwner = "demo-owner",
        prjEnableDynRunCmd = False,
        prjDeclSpecFile = Nothing,
        prjDeclInputType = Nothing
      }
