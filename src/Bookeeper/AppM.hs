module Bookeeper.AppM
  ( AppM(..)
  , runAppM
  ) where


import Protolude hiding (Handler)

import Servant
import Data.Vector hiding (forM_)
import Control.Monad.RWS hiding (forM_)

import Bookeeper.Util


type AppM' = RWST Env (Vector Text) () Handler

newtype AppM a = AppM { unAppM :: AppM' a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadError ServerError
           , MonadReader Env
           , MonadWriter (Vector Text)
           , MonadState ()
           , MonadRWS Env (Vector Text) ()
           )
       via AppM'


runAppM :: Env -> AppM a -> Handler a
runAppM env appM = do
  (a, _, w) <- runRWST (unAppM appM) env ()

  forM_ w putStrLn

  pure a
