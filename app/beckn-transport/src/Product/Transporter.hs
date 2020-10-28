{-# LANGUAGE OverloadedLabels #-}

module Product.Transporter where

import App.Types
import Beckn.TypeClass.Transform
import Beckn.Types.App
import qualified Beckn.Types.Storage.Organization as SO
import qualified Beckn.Types.Storage.Person as SP
import qualified Beckn.Types.Storage.RegistrationToken as SR
import Beckn.Utils.Common
import EulerHS.Prelude
import qualified Storage.Queries.Organization as QO
import qualified Storage.Queries.Person as QP
import Types.API.Transporter

createTransporter :: SR.RegistrationToken -> TransporterReq -> FlowHandler TransporterRes
createTransporter SR.RegistrationToken {..} req = withFlowHandler $ do
  person <- QP.findPersonById (PersonId _EntityId)
  validate person
  organization <- createTransform req
  validateReq req
  QO.create organization
  QP.updateOrganizationIdAndMakeAdmin (PersonId _EntityId) (_getOrganizationId $ SO._id organization)
  updatedPerson <- QP.findPersonById (PersonId _EntityId)
  return $ TransporterRes updatedPerson organization
  where
    validate person = do
      unless (SP._verified person) $
        throwError400 "user not verified"
      when (isJust $ SP._organizationId person) $
        throwError400 "user already registered an organization"
      when (SP._role person /= SP.ADMIN) $
        throwError401 "unauthorized"
    validateReq treq =
      unless (all (== True) (isJust <$> transporterMandatoryFields treq)) $
        throwError400 "missing mandatory fields"

createGateway :: SO.Organization -> TransporterReq -> FlowHandler GatewayRes
createGateway _ req = withFlowHandler $ do
  organization <- createTransform req
  QO.create organization
  return $ TransporterRec organization

updateTransporter :: SR.RegistrationToken -> Text -> UpdateTransporterReq -> FlowHandler TransporterRec
updateTransporter SR.RegistrationToken {..} orgId req = withFlowHandler $ do
  maybePerson <- QP.findPersonByIdAndRoleAndOrgId (PersonId _EntityId) SP.ADMIN orgId
  now <- getCurrTime
  case maybePerson of
    Just person -> do
      validate person
      org <- QO.findOrganizationById $ OrganizationId orgId
      organization <-
        if req ^. #enabled /= Just False
          then modifyTransform req org >>= addTime (Just now)
          else modifyTransform req org
      QO.updateOrganizationRec organization
      return $ TransporterRec organization
    Nothing -> throwError400 "user not eligible"
  where
    validate person =
      unless (SP._verified person) $ throwError400 "user not verified"
    addTime fromTime org =
      return $ org {SO._fromTime = fromTime}

getTransporter :: SR.RegistrationToken -> FlowHandler TransporterRec
getTransporter SR.RegistrationToken {..} = withFlowHandler $ do
  person <- QP.findPersonById (PersonId _EntityId)
  validate person
  case person ^. #_organizationId of
    Just orgId -> TransporterRec <$> QO.findOrganizationById (OrganizationId orgId)
    Nothing -> throwError400 "user not registered an organization"
  where
    validate person =
      unless (SP._verified person) $ throwError400 "user not verified"

transporterMandatoryFields :: TransporterReq -> [Maybe Text]
transporterMandatoryFields req =
  [ req ^. #_mobileNumber,
    req ^. #_mobileCountryCode,
    req ^. #_district,
    req ^. #_city,
    req ^. #_country
  ]
