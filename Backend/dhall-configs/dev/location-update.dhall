let common = ./common.dhall

let sec = ./secrets/dynamic-offer-driver-app.dhall

let appCfg = ./dynamic-offer-driver-app.dhall

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5434
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_driver_offer_bpp"
      }

let esqDBReplicaCfg =
      { connectHost = esqDBCfg.connectHost
      , connectPort = esqDBCfg.connectPort
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
      }

let hedisCfg =
      { connectHost = "localhost"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      }

let consumerProperties =
      { groupId = "groupId"
      , brockers = [ "localhost:29092" ]
      , autoCommit = None Integer
      }

let kafkaConsumerCfg =
      { topicNames = [ "location-updates" ], consumerProperties }

let availabilityTimeWindowOption =
      { period = +7, periodType = common.periodType.Days }

let cacheConfig = { configsExpTime = +86400 }

let healthCheckAppCfg =
      { graceTerminationPeriod = appCfg.graceTerminationPeriod
      , healthcheckPort = +8115
      , notificationMinDelay = +50000
      , driverInactiveDelay = +86400
      , smsCfg = appCfg.smsCfg
      , driverInactiveSmsTemplate =
          "Alert! You have been marked Busy on Namma Yatri Partner, as we have not received any location update from your phone in more than a day. Please open the app and update your location for the app to work properly."
      , driverAllowedDelayForLocationUpdateInSec = +60
      , driverLocationHealthCheckIntervalInSec = +120
      , loggerConfig =
              appCfg.loggerConfig
          //  { logFilePath = "/tmp/driver-tracking-healthcheck.log" }
      }

in  { hedisCfg
    , esqDBCfg
    , esqDBReplicaCfg
    , cacheConfig
    , dumpEvery = +10
    , kafkaConsumerCfg
    , timeBetweenUpdates = +10
    , availabilityTimeWindowOption
    , granualityPeriodType = common.periodType.Hours
    , httpClientOptions = common.httpClientOptions
    , metricsPort = +9994
    , encTools = appCfg.encTools
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/kafka-consumers.log", logRawSql = False }
    , healthCheckAppCfg = Some healthCheckAppCfg
    }
