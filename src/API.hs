{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module API where

import Data.Aeson
import GHC.Exts
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant


data User = User
  { userId        :: Int
  , userFirstName :: Text
  , userLastName  :: Text
  } deriving (Show, Generic)
instance FromJSON User
instance ToJSON User


data SortBy = SortById | SortByName deriving Eq


instance FromHttpApiData SortBy where
  parseQueryParam txt = case txt of
                          "name" -> Right SortByName
                          "id"   -> Right SortById
                          _      -> Left "No sort by passed"


-- | GET user API endpoint.
type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]
          :<|> "user" :> Capture "uid" Int :> Get '[JSON] User


-- | User database.
users :: [User]
users = [ User 1 "Assumption" "Bulltron"
        , User 5 "Destiny" "Frankenstein"
        , User 25 "Kobe" "Buffalomeat"
        , User 125 "Jimbob" "Ghostkeeper"
        , User 625 "Dr. Narwhals" "Mating"
        ]


-- | Handler for the GET users endpoint.
getUsers :: Maybe SortBy -> Handler [User]
getUsers maybeTheSort = case maybeTheSort of
                          Just SortById   -> return $ sortWith userId users
                          Just SortByName -> return $ sortWith userFirstName users
                          Nothing         -> return users
                          

-- | Handler for the GET user endpoint.
getUser :: Int -> Handler User
getUser uid = do
  let user = [u | u <- users, userId u == uid]
  case user of
    (u:_) -> return u
    _     -> throwError err404 { errBody = "User not found" }
