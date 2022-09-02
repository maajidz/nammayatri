let common = ./common.dhall
let sec = ./secrets/bpp-dashboard.dhall

let esqDBCfg =
  { connectHost = "localhost"
  , connectPort = 5434
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_dev"
  , connectSchemaName = "atlas_bpp_dashboard"
  }

let rcfg =
  { connectHost = "localhost"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +0
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = None Integer
  }

let apiRateLimitOptions =
  { limit = +4
  , limitResetTimeInSec = +600
  }

let encTools =
  { service = common.passetto
  , hashSalt = sec.encHashSalt
  }

in

{ esqDBCfg = esqDBCfg
, redisCfg = rcfg
, port = +8018
, migrationPath = Some (env:BPP_DASHBOARD_MIGRATION_PATH as Text ? "dev/migrations/bpp-dashboard")
, autoMigrate = True
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/bpp-dashboard.log"}
, graceTerminationPeriod = +90
, apiRateLimitOptions = apiRateLimitOptions
, httpClientOptions = common.httpClientOptions
, authTokenCacheExpiry = +600 --seconds
, registratinTokenExpiry = +365 --days
, encTools = encTools
}
