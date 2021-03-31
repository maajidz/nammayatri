let common = ./common.dhall
let sec = ./secrets/mock-app-backend.dhall

let postgresConfig =
  { connectHost = "beckn-sandbox-v2.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_mock_app_backend_v2"
  }

let pgcfg =
  { connTag = "providerDb"
  , pgConfig = postgresConfig
  , poolConfig = common.defaultPoolConfig
  , schemaName = "atlas_mock_app_backend"
  }

let gwUri = "http://beckn-gateway-${common.branchName}.atlas:8015/v1"

in

{ dbCfg = pgcfg
, port = +8016
, metricsPort = +9999
, xGatewayUri = gwUri
, selfId = "JUSPAY.MOCK.FMD.BAP.DEV"
, nwAddress = "https://api.sandbox.beckn.juspay.in/dev/mock/app/v1/"
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/mock-app-backend.log"}
, credRegistry = common.credRegistry
, signingKeys = common.signingKeys
, signatureExpiry = common.signatureExpiry
}
