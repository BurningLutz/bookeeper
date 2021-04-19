module Bookeeper.Server.Book
  ( bookServer
  ) where


import Protolude
import qualified Protolude.Partial as Partial

import Data.Pool
import Control.Lens
import Opaleye
import Servant
import Servant.Auth.Server
import Database.PostgreSQL.Simple.Errors

import Bookeeper.AppM
import Bookeeper.API
import Bookeeper.Model
import Bookeeper.Query


bookServer :: ServerT BookAPI AppM
bookServer = adminBookServer
        :<|> publicBookServer

  where
    adminBookServer (Authenticated ClaimAdmin {}) = addBook
                                               :<|> setBook
                                               :<|> delBook
      where
        addBook :: AddBook -> AppM Int64
        addBook AddBook {..} = do
          Env { pool } <- ask

          let
            newBook = newEntity Book { _sn, _title, _author }

          ei <- liftIO $ withResource pool \conn -> do
            catchViolation (uniqError "books_sn_key") do
              Right <$> runInsert_ conn Insert
                { iTable = books
                , iRows = [newBook]
                , iReturning = rReturning (^.id)
                , iOnConflict = Nothing
                }

          ei & either throwError (pure . Partial.head)

        setBook :: Int64 -> SetBook -> AppM NoContent
        setBook bid SetBook {..} = do
          Env { pool } <- ask

          n <- liftIO $ withResource pool \conn -> do
            runUpdate_ conn Update
              { uTable = books
              , uWhere = \u -> u^.id .== toFields bid
              , uUpdateWith = modEntity ( (title .~ toFields _title)
                                        . (author .~ toFields _author)
                                        )
              , uReturning = rCount
              }

          if n > 0
             then pure NoContent
             else throwError err404

        delBook :: Int64 -> AppM NoContent
        delBook bid = do
          Env { pool } <- ask

          ei <- liftIO $ withResource pool \conn -> do
            catchViolation (fkeyError "borrowings_book_id_fkey") do
              Right <$> runDelete_ conn Delete
                { dTable = books
                , dWhere = \b -> b^.id .== toFields bid
                , dReturning = rCount
                }

          ei & either throwError \n ->
            if n > 0
               then pure NoContent
               else throwError err404

    adminBookServer _ = throwAll err401

    publicBookServer = getBooks
      where
        getBooks :: AppM [Book]
        getBooks = do
          Env { pool } <- ask

          liftIO $ withResource pool \conn -> do
            runSelect conn (selectTable books)
