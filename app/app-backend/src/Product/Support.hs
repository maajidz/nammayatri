{-# LANGUAGE OverloadedStrings #-}

module Product.Support
  ( sendIssue,
  )
where

import qualified App.Types as App
import qualified Beckn.SesConfig as SesConfig
import Beckn.Types.Common
import Beckn.Types.Id
import qualified Beckn.Types.Storage.Issue as SIssue
import Beckn.Types.Storage.Person as Person
import Data.Time (UTCTime)
import qualified EulerHS.Language as L
import EulerHS.Prelude hiding (length)
import qualified Storage.Queries.Issues as Queries
import qualified Types.API.Common as API
import Types.API.Support as Support
import Utils.Common
import qualified Utils.SES as SES

sendIssue :: Person.Person -> Support.SendIssueReq -> App.FlowHandler Support.SendIssueRes
sendIssue person request@SendIssueReq {..} = withFlowHandlerAPI $ do
  let personId = getId $ person.id
  issuesConfig <- asks $ SesConfig.issuesConfig . App.sesCfg
  issueId <- L.generateGUID
  utcNow <- getCurrentTime
  Queries.insertIssue (mkDBIssue issueId personId request utcNow)
  let mailSubject = mkMailSubject issueId (issue.reason)
  let mailBody = mkMailBody issueId personId request utcNow
  responseError <- L.runIO $ SES.sendEmail issuesConfig mailSubject mailBody
  case responseError of
    Just resError -> do
      logTagInfo "SES" resError
      pure $ API.Ack {action = "Error", message = resError}
    Nothing -> pure $ API.Ack {action = "Successful", message = ""}

mkDBIssue :: Text -> Text -> Support.SendIssueReq -> UTCTime -> SIssue.Issue
mkDBIssue issueId customerId SendIssueReq {..} time =
  SIssue.Issue
    { id = Id issueId,
      customerId = Id customerId,
      productInstanceId = Id <$> productInstanceId,
      contactEmail = contactEmail,
      reason = issue.reason,
      description = issue.description,
      createdAt = time,
      updatedAt = time
    }

mkMailSubject :: Text -> Text -> Text
mkMailSubject issueId reason = "Issue " <> issueId <> ". " <> reason

mkMailBody :: Text -> Text -> Support.SendIssueReq -> UTCTime -> Text
mkMailBody issueId personId SendIssueReq {..} time =
  "Issue id: " <> issueId
    <> "\nPerson id: "
    <> personId
    <> "\nOrder id: "
    <> orderId
    <> "\nContact email: "
    <> contactEmail
    <> "\nCreation time: "
    <> show time
    <> "\n\nReason: "
    <> issue.reason
    <> "\n\nDetails: "
    <> description
  where
    orderId = fromMaybe "issue does not belong to specific order." productInstanceId
    description = fromMaybe "no details provided by user." (issue.description)
