{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}

module Main where

import Control.Monad.Reader
import Control.Monad.IO.Class

import Database.Beam
import Database.Beam.Sqlite
import Database.SQLite.Simple

import Data.Text(Text)

data UserT f = User
  { _userEmail     :: Columnar f Text
  , _userFirstName :: Columnar f Text
  , _userLastName  :: Columnar f Text
  , _userPassword  :: Columnar f Text }
   deriving Generic

type User = UserT Identity
type UserId = PrimaryKey UserT Identity

deriving instance Show User
deriving instance Eq User

instance Beamable UserT

instance Table UserT where
    data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
    primaryKey = UserId . _userEmail
instance Beamable (PrimaryKey UserT)

data ShoppingCartDb f = ShoppingCartDb
 { _shoppingCartUsers :: f (TableEntity UserT)
 } deriving Generic

instance Database ShoppingCartDb

shoppingCartDb :: DatabaseSettings be ShoppingCartDb
shoppingCartDb = defaultDbSettings


type DBIO m = (MonadReader Connection m, MonadIO m)

withConn
  :: (DBIO m1, MonadBeam syntax be Connection m2)
  => m2 b -> m1 b
withConn f = do
  conn <- ask
  liftIO $ withDatabase conn f

insertUsers :: DBIO m => m ()
insertUsers = withConn . runInsert $
    insert (_shoppingCartUsers shoppingCartDb) $
      insertValues
        [ User
          "james@example.com"
          "James"
          "Smith"
          "b4cc344d25a2efe540adbf2678e2304c"
        , User
          "betty@example.com"
          "Betty"
          "Jones"
          "82b054bd83ffad9b6cf8bdb98ce3cc2f"
        , User
          "sam@example.com"
          "Sam"
          "Taylor"
          "332532dcfaa1cbf61e2a266bd723612c"
        , User
          "sam@jely.com"
          "Sam"
          "Jely"
          "332532dcfaa1cbf61e2a266bd723612c"
        , User
          "james@oreily.com"
          "James"
          "O'Reily"
          "b4cc344d25a2efe540adbf2678e2304c"
        ]

numUsersByName :: DBIO m => m [(Text, Int)]
numUsersByName = withConn . runSelectReturningList . select $
  aggregate_ (\u -> (group_ (_userFirstName u), countAll_)) $
    all_ (_shoppingCartUsers shoppingCartDb)

numUsers :: DBIO m => m (Maybe Int)
numUsers = withConn . runSelectReturningOne . select $
  aggregate_ (\_ -> countAll_)
             (all_ (_shoppingCartUsers shoppingCartDb))

runSql :: DBIO m => m ()
runSql = do
  count <- numUsers
  case count of
    Just 0 -> insertUsers
    _ -> pure ()
  byName <- numUsersByName
  mapM_ (liftIO . putStrLn . show) byName

main :: IO ()
main = open "shoppingcart1.db" >>= runReaderT runSql
