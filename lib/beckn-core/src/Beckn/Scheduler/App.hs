module Beckn.Scheduler.App
  ( runScheduler,
    createJob,
    createJobIn,
    emptyCatchers,
  )
where

import Beckn.Mock.Utils (threadDelaySec)
import Beckn.Prelude
import Beckn.Scheduler.Environment
import Beckn.Scheduler.Error
import Beckn.Scheduler.JobHandler
import Beckn.Scheduler.Serialization
import Beckn.Scheduler.Storage.Queries
import qualified Beckn.Scheduler.Storage.Queries as Q
import Beckn.Scheduler.Types
import Beckn.Storage.Esqueleto
import Beckn.Storage.Esqueleto.Config (prepareEsqDBEnv)
import qualified Beckn.Storage.Esqueleto.Queries as Esq
import qualified Beckn.Storage.Esqueleto.Transactionable as Esq
import Beckn.Storage.Hedis (connectHedis)
import qualified Beckn.Storage.Hedis.Queries as Hedis
import Beckn.Types.Common
import Beckn.Types.Error (GenericError (InternalError))
import Beckn.Types.Id
import Beckn.Utils.App (getPodName)
import Beckn.Utils.Common
import Beckn.Utils.IOLogging (prepareLoggerEnv)
import qualified Control.Monad.Catch as C
import qualified Data.Map as Map
import UnliftIO.Concurrent (forkIO)

runScheduler ::
  forall t m.
  JobTypeSerializable t =>
  C.MonadThrow m =>
  SchedulerConfig ->
  (forall q. SchedulerResources -> m q -> IO q) ->
  JobHandlerList m t ->
  IO ()
runScheduler SchedulerConfig {..} runMonad handlersList = do
  hostname <- getPodName
  loggerEnv <- prepareLoggerEnv loggerConfig hostname
  esqDBEnv <- prepareEsqDBEnv esqDBCfg loggerEnv
  hedisEnv <- connectHedis hedisCfg (\k -> hedisPrefix <> ":" <> k)
  let schedulerResources = SchedulerResources {..}
      transformFunc :: forall q. m q -> IO q
      transformFunc = runMonad schedulerResources
      handlersMap = Map.fromList $ map (second $ transformJobHandler transformFunc) handlersList
  let schedulerEnv = SchedulerEnv {..}
  runSchedulerM schedulerEnv runner

runner :: (JobTypeSerializable t) => SchedulerM t ()
runner = do
  before <- getCurrentTime
  runnerIteration
  after <- getCurrentTime
  let diff = floor $ abs $ diffUTCTime after before
  loopIntervalSec <- asks (.loopIntervalSec)
  threadDelaySec (loopIntervalSec - diff)
  runner

runnerIteration :: (JobTypeSerializable t) => SchedulerM t ()
runnerIteration = do
  readyTasks <- getReadyTasks
  availableReadyTasksIds <- filterM attemptTaskLock $ map (.id) readyTasks
  takenTasksUpdatedInfo <- getTasksById availableReadyTasksIds
  let withLogTag' job = withLogTag ("JobId=" <> job.id.getId)
  mapM_ (\job -> forkIO $ withLogTag' job $ executeTask job) takenTasksUpdatedInfo

attemptTaskLock :: Id (Job a b) -> SchedulerM t Bool
attemptTaskLock jobId = do
  expirationTime <- asks (.expirationTime)
  successfulSet <- Hedis.setNx jobId.getId ()
  if successfulSet
    then do
      Hedis.expire jobId.getId expirationTime
      pure True
    else pure False

failJob :: JobText -> Text -> SchedulerM t ()
failJob jobText description = do
  logError $ "failed to execute job: " <> description
  logPretty ERROR "failed job" jobText
  markAsTerminated jobText.id

withJobDataDecoded :: forall d t m. (JobDataSerializable d, Log m, Monad m) => Job t Text -> (Job t d -> m ExecutionResult) -> m ExecutionResult
withJobDataDecoded txtDataJob action =
  maybe errHandler successHandler $ jobDataFromText @d txtDataJob.jobData
  where
    errHandler = do
      logError $ "failed to decode job data: " <> txtDataJob.jobData
      pure Terminate
    successHandler jobData_ = action $ setJobData jobData_ txtDataJob

executeTask :: forall t. (JobTypeSerializable t) => JobText -> SchedulerM t ()
executeTask rawJob = do
  let eithTypeDecodedJob = decodeJob @t @Text rawJob
  case eithTypeDecodedJob of
    Left err -> failJob rawJob $ "type decode failure: " <> show err
    Right decJob -> do
      hMap <- asks (.handlersMap)
      let decJobType = decJob.jobType
      case Map.lookup decJobType hMap of
        Nothing -> failJob rawJob $ "no handler function for the job type = " <> show decJobType
        Just jH -> executeTypeDecodedJob jH decJob
  where
    executeTypeDecodedJob :: JobHandler IO t -> Job t Text -> SchedulerM t ()
    executeTypeDecodedJob jH@(JobHandler handlerFunc_ errorCatchers_) job = do
      result <- withJobDataDecoded job $ \decJob -> do
        let totalErrorCatchers = errorCatchers_ decJob ++ [resultCatcher, defaultCatcher]
        liftIO $ handlerFunc_ decJob `C.catches` totalErrorCatchers
      case result of
        Completed -> do
          logInfo $ "job successfully completed on try " <> show (job.currErrors + 1)
          markAsComplete job.id
        Terminate -> do
          logInfo $ "job terminated on try " <> show (job.currErrors + 1)
          markAsTerminated job.id
        ReSchedule reScheduledTime -> do
          logInfo $ "job rescheduled on time = " <> show reScheduledTime
          reSchedule job.id reScheduledTime
        Retry ->
          let newErrorsCount = job.currErrors + 1
           in if newErrorsCount >= job.maxErrors
                then do
                  logError $ "retries amount exceeded, job failed after try " <> show newErrorsCount
                  updateErrorCountAndTerminate job.id newErrorsCount
                else do
                  logInfo $ "try " <> show newErrorsCount <> " was not successful, trying again"
                  updateFailureCount job.id newErrorsCount
                  waitBeforeRetry <- asks (.waitBeforeRetry)
                  threadDelaySec waitBeforeRetry
                  executeTypeDecodedJob jH job {currErrors = newErrorsCount}

resultCatcher :: C.Handler IO ExecutionResult
resultCatcher = C.Handler pure

defaultCatcher :: C.Handler IO ExecutionResult
defaultCatcher = C.Handler $ const @_ @SomeException $ pure Retry

emptyCatchers :: Job t d -> [C.Handler m b]
emptyCatchers = const []

-- api

createJobIn ::
  ( HasEsqEnv r m,
    MonadGuid m,
    MonadCatch m,
    JobTypeSerializable a,
    JobDataSerializable b,
    Show a,
    Show b
  ) =>
  NominalDiffTime ->
  JobEntry a b ->
  m (Either JobDecodeError (Id (Job a b)))
createJobIn diff jobEntry = do
  now <- getCurrentTime
  let scheduledAt = addUTCTime diff now
  createJob scheduledAt jobEntry

createJob ::
  forall a b r m.
  ( HasEsqEnv r m,
    MonadGuid m,
    MonadCatch m,
    JobTypeSerializable a,
    JobDataSerializable b,
    Show a,
    Show b
  ) =>
  UTCTime ->
  JobEntry a b ->
  m (Either JobDecodeError (Id (Job a b)))
createJob scheduledAt jobEntry = do
  now <- getCurrentTime
  id <- Id <$> generateGUIDText
  let job = makeJob id now
      jobText = encodeJob job
  eithUnit <- C.try $
    Esq.runTransaction $ do
      Q.create jobText
      mbFetchedJob <- Esq.findById jobText.id
      fetchedJob <- fromMaybeM (InternalError "Failed to insert job") mbFetchedJob
      case decodeJob @a @b fetchedJob of
        Left err -> do
          logError $ "failed to decode job:" <> show fetchedJob
          throwError err
        Right decodedJob ->
          unless (typeAndDataAreEqual job decodedJob) $
            logWarning $ "database representations of the inserted and the fetched jobs are not equal: " <> show job <> " : " <> show decodedJob
      void $ fromEitherM identity $ decodeJob @a @b fetchedJob
  either (pure . Left) (\_ -> pure $ Right job.id) eithUnit
  where
    typeAndDataAreEqual job1 job2 = job1.jobData == job2.jobData && job1.jobType == job2.jobType
    makeJob id currentTime =
      Job
        { id = id,
          jobType = jobEntry.jobType,
          jobData = jobEntry.jobData,
          scheduledAt = scheduledAt,
          maximumDelay = jobEntry.maximumDelay,
          maxErrors = jobEntry.maxErrors,
          createdAt = currentTime,
          updatedAt = currentTime,
          currErrors = 0,
          status = PENDING
        }
