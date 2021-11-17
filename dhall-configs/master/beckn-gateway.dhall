let common = ./common.dhall
let sec = ./secrets/beckn-gateway.dhall

let postgresConfig =
  { connectHost = "beckn-sandbox-v2.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_gateway_v2"
  }

let pgcfg =
  { connTag = "gatewayDb"
  , pgConfig = postgresConfig
  , poolConfig = common.defaultPoolConfig
  , schemaName = "atlas_gateway"
  }

let esqDBCfg = 
  { connectHost = postgresConfig.connectHost
  , connectPort = postgresConfig.connectPort
  , connectUser = postgresConfig.connectUser
  , connectPassword = postgresConfig.connectPassword
  , connectDatabase = postgresConfig.connectDatabase
  , connectSchemaName = pgcfg.schemaName
  }

let rcfg =
  { connectHost = "ec-redis-beta.bfw4iw.ng.0001.apse1.cache.amazonaws.com"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +1
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = Some +100
  }

let httpClientOptions =
  { timeoutMs = +2000
  , maxRetries = +3
  }

let coreVersions =
  { mobility = "0.8.2"
  , finalMileDelivery = "0.9.1"
  , localRetail = "0.9.1"
  , foodAndBeverage = "0.9.1"
  }

in

{ dbCfg = pgcfg
, esqDBCfg = esqDBCfg
, redisCfg = rcfg
, port = +8015
, metricsPort = +9999
, selfId = "api.sandbox.beckn.juspay.in/dev/gateway/v1"
, hostName = "juspay.in"
, nwAddress = "https://api.sandbox.beckn.juspay.in/dev/gateway/v1/"
, credRegistry = common.credRegistry
, signingKeys = common.signingKeys
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, searchTimeout = Some +3600
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/beckn-gateway.log"}
, coreVersions = coreVersions
, mobilityDomainVersion = "0.8.2"
, signatureExpiry = common.signatureExpiry
, graceTerminationPeriod = +90
, httpClientOptions = httpClientOptions
, registryUrl = common.registryUrl
, registrySecrets = sec.registrySecrets
}
