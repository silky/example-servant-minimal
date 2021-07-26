{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

-- * api

type SomeApi t =
  "api" :> "new" :> "contract" :>
    ("activate" :> ReqBody '[JSON] t :> Post '[JSON] Integer
      :<|> "instance" :>
          (Capture "something" String :>
            ("status" :> Get '[JSON] String
            :<|> "endpoint" :> Capture "name" String :> ReqBody '[JSON] Value :> Post '[JSON] ()
            :<|> "stop" :> Put '[JSON] ()
            )
          )
      :<|> "instances" :> "wallet" :> Capture "wallet-id" Integer :> Get '[JSON] [t]
      :<|> "instances" :> Get '[JSON] [t]
      :<|> "definitions" :> Get '[JSON] [t]
    )

type ItemApi =
  "thing" :> ReqBody '[JSON] Integer :> Post '[JSON] Integer :<|>
  "item" :> Get '[JSON] [Item] :<|>
  "item" :> Capture "itemId" Integer :> Get '[JSON] Item

itemApi :: Proxy ItemApi
itemApi = Proxy

someApi :: Proxy (SomeApi Integer)
someApi = Proxy

someServer :: Server (SomeApi Integer)
someServer =
  (\i -> pure $ i + 50)
  :<|> (\s ->
      (pure $ s ++ s)
      :<|> (\_ _ -> pure ())
      :<|> (pure ())
    )
  :<|> (\wi -> pure [wi])
  :<|> (pure [1])
  :<|> (pure [2])

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve someApi someServer
-- mkApp = return $ serve itemApi server

server :: Server ItemApi
server =
  thing :<|>
  getItems :<|>
  getItemById

thing :: Integer -> Handler Integer
thing i = pure (i + 10)

getItems :: Handler [Item]
getItems = return [exampleItem]

getItemById :: Integer -> Handler Item
getItemById = \ case
  0 -> return exampleItem
  _ -> throwError err404

exampleItem :: Item
exampleItem = Item 0 "example item"

-- * item

data Item
  = Item {
    itemId :: Integer,
    itemText :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item

data a + b = Foo a b

type X = Int + Bool
