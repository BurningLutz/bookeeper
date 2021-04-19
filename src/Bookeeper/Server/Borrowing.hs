module Bookeeper.Server.Borrowing
  ( borrowingServer
  ) where


import Protolude
import qualified Protolude.Partial as Partial

import Data.Time
import Data.Pool
import Control.Lens
import Opaleye
import Servant
import Servant.Auth.Server

import Bookeeper.AppM
import Bookeeper.API
import Bookeeper.Model
import Bookeeper.Query


borrowingServer :: ServerT BorrowingAPI AppM
borrowingServer = adminBorrowingServer
             :<|> userBorrowingServer

  where
    adminBorrowingServer (Authenticated ClaimAdmin {}) = getBorrowings
                                        :<|> setBorrowing
      where
        value_ = sets \f s -> s { _value = f (_value s) }
        book_  = sets \f s -> s { _book  = f (_book s) }
        user_  = sets \f s -> s { _user  = f (_user s) }

        getBorrowings :: AppM [BorrowingDetail]
        getBorrowings = do
          Env { pool } <- ask

          liftIO $ withResource pool \conn -> do
            runSelect conn do
              borrowingR <- selectTable borrowings
              bookR <- selectTable books
              userR <- selectTable users

              viaLateral restrict ( (borrowingR^.value.user) .== (userR^.id)
                                .&& (borrowingR^.value.book) .== (bookR^.id)
                                  )

              pure $ borrowingR & ( (value_.book_ .~ bookR)
                                  . (value_.user_ .~ userR)
                                  )

        setBorrowing :: Int64 -> SetBorrowing -> AppM NoContent
        setBorrowing bid SetBorrowing {..} = do
          Env { pool } <- ask

          n <- liftIO $ withResource pool \conn -> do
            runUpdate_ conn Update
              { uTable = borrowings
              , uWhere = \u -> u^.id .== toFields bid
              , uUpdateWith = modEntity (status .~ toFields _status)
              , uReturning = rCount
              }

          if n > 0
             then pure NoContent
             else throwError err404

    adminBorrowingServer _ = throwAll err401

    userBorrowingServer (Authenticated ClaimUser {..}) = borrow
                                                    :<|> getMyBorrowings
      where
        borrow :: AddBorrowing -> AppM Int64
        borrow AddBorrowing {..} = do
          Env { pool } <- ask
          now <- liftIO getCurrentTime

          let
            borrowing = newEntity Borrowing
              { _book = _bookId
              , _user = _id
              , _date = now
              , _status = Pending
              }

          ret <- liftIO $ withResource pool \conn -> do
            runInsert_ conn Insert
              { iTable = borrowings
              , iRows = [borrowing]
              , iReturning = rReturning (^.id)
              , iOnConflict = Nothing
              }

          pure $ Partial.head ret

        getMyBorrowings :: AppM [Borrowing]
        getMyBorrowings = do
          Env { pool } <- ask

          liftIO $ withResource pool \conn -> do
            runSelect conn do
              borrowing <- selectTable borrowings

              viaLateral restrict (borrowing^.value.user .== toFields _id)

              pure borrowing

    userBorrowingServer _ = throwAll err401
