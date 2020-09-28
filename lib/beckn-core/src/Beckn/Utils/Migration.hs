{-# LANGUAGE TypeApplications #-}

module Beckn.Utils.Migration
  ( migrateIfNeeded,
  )
where

import Beckn.Storage.DB.Config
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.Migration
import EulerHS.Prelude
import qualified EulerHS.Types as T

connect :: T.PostgresConfig -> IO PS.Connection
connect T.PostgresConfig {..} = PS.connect PS.ConnectInfo {..}

migrateIfNeeded :: Maybe FilePath -> DBConfig -> Bool -> IO (Either String ())
migrateIfNeeded mPath dbCfg autoMigrate =
  case mPath of
    Just path | autoMigrate ->
      fmap resultToEither $ do
        putStrLn @String $ "Running migrations (" <> path <> ") ..."
        conn <- connect $ pgConfig dbCfg
        PS.withTransaction conn $
          runMigrations
            True
            conn
            [ MigrationInitialization,
              MigrationDirectory path
            ]
    _ ->
      pure $ Right ()
  where
    resultToEither MigrationSuccess = Right ()
    resultToEither (MigrationError a) = Left a
