module Bookeeper.AppM
  ( AppM(..)
  , runAppM
  ) where


import Protolude hiding (Handler)

import Servant
import Data.Vector hiding (forM_)
import Control.Monad.RWS hiding (forM_)


type AppM' = RWST Text (Vector Text) () Handler

newtype AppM a = AppM { unAppM :: AppM' a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadError ServerError
           , MonadReader Text
           , MonadWriter (Vector Text)
           , MonadState ()
           , MonadRWS Text (Vector Text) ()
           )
       via AppM'


runAppM :: AppM a -> Handler a
runAppM appM = do
  (a, _, w) <- runRWST (unAppM appM) "" ()

  forM_ w putStrLn

  pure a
