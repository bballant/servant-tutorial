module Server where

import           API
import           Config
import           Config                      (Config (..), Environment (..),
                                              makePool, setLogger)
import qualified Control.Monad.Metrics       as M
import           Data.Text (Text)
import           Logger                      (defaultLogEnv)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Metrics         (metrics, registerWaiMetrics)
import           Safe                        (readMay)
import           Servant
import           System.Remote.Monitoring    (forkServer, serverMetricStore,
                                              serverThreadId)
import           System.Environment          (lookupEnv)


-- | Full API specification.
type API = Get '[JSON] Text :<|>  UserAPI

-- | Serve the API.
server :: Server API
server = home :<|> getUsers :<|> getUser
  where
    home = return "User API"

-- | To guide type inference.
api :: Proxy API
api = Proxy

-- | A WAI application.
app :: Application
app = serve api server

-- | Looks up a setting in the environment, with a provided default, and
-- 'read's that information into the inferred type.
lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing ->
            return def
        Just str ->
            maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str =
        error $ mconcat 
            [ "Failed to read [["
            , str
            , "]] for environment variable "
            , env
            ]

-- | Allocates resources for 'Config'
mkConfig :: IO Config
mkConfig = do
    port      <- lookupSetting "PORT" 8081
    env       <- lookupSetting "ENV" Development
    logEnv    <- defaultLogEnv
    pool      <- makePool env logEnv
    ekgServer <- forkServer "localhost" 8000
    let store = serverMetricStore ekgServer
    waiMetrics <- registerWaiMetrics  store
    metr       <- M.initializeWith store
    pure Config
        { configPool = pool
        , configEnv = env
        , configMetrics = metr
        , configLogEnv = logEnv
        , configPort = port
        , configEkgServer = serverThreadId ekgServer
        }

startApp1000 :: IO ()
startApp1000 = run 1234 app

-- | Run the application.
startApp :: IO ()
startApp = run 1234 app
