module Bookeeper.AppM
  ( AppM(..)
  , runAppM
  ) where


import Protolude hiding (Handler)

import Servant
import Control.Monad.RWS hiding (forM_)

import Bookeeper.Model


type AppM' = RWST Env [Text] () Handler

newtype AppM a = AppM { unAppM :: AppM' a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadError ServerError
           , MonadReader Env
           , MonadWriter [Text]
           , MonadState ()
           , MonadRWS Env [Text] ()
           )
       via AppM'


runAppM :: Env -> AppM a -> Handler a
runAppM env appM = do
  (a, _, w) <- runRWST (unAppM appM) env ()

  forM_ w putStrLn

  pure a
