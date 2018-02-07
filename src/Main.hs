{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.Beam
import Database.Beam.Backend.Sqlite3

import Data.Text (Text)

-- data UserT f = User
--   { _userEmail     :: Columnar f Text
--   , _userFirstName :: Columnar f Text
--   , _userLastName  :: Columnar f Text
--   , _userPassword  :: Columnar f Text }
--    deriving Generic
--
-- type User = UserT Identity
-- deriving instance Show User

-- instance Table UserT where
--   data PrimaryKey UserT f = UserId (Columnar f Text) deriving Generic
--   primaryKey = UserId . _userEmail
--
-- type UserId = PrimaryKey UserT Identity
-- deriving instance Show UserId

 main :: IO ()
 main = putStrLn "Hello, Haskell!"
