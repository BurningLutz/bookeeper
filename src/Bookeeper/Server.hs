module Bookeeper.Server
  ( server
  ) where


import Protolude
import Servant

import Bookeeper.API
import Bookeeper.AppM
import Bookeeper.Server.User
import Bookeeper.Server.RPC


server :: ServerT FullAPI AppM
server = userServer
    :<|> rpcServer
