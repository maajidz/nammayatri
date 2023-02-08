{-# LANGUAGE TypeApplications #-}

module Storage.Queries.Role where

import Domain.Types.Role as Role
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Storage.Tabular.Role

create :: Role -> SqlDB ()
create = Esq.create

findById ::
  Transactionable m =>
  Id Role ->
  m (Maybe Role)
findById = Esq.findById

findByName ::
  Transactionable m =>
  Text ->
  m (Maybe Role)
findByName name = findOne $ do
  role <- from $ table @RoleT
  where_ $
    role ^. RoleName ==. val name
  return role

findAllByLimitOffset ::
  Transactionable m =>
  Maybe Integer ->
  Maybe Integer ->
  m [Role]
findAllByLimitOffset mbLimit mbOffset = do
  let limitVal = fromIntegral $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
  Esq.findAll $ do
    role <- from $ table @RoleT
    orderBy [asc $ role ^. RoleName]
    limit limitVal
    offset offsetVal
    pure role

findAllWithLimitOffset ::
  Transactionable m =>
  Maybe Integer ->
  Maybe Integer ->
  Maybe Text ->
  m [Role]
findAllWithLimitOffset mbLimit mbOffset mbSearchString = do
  let limitVal = fromIntegral $ fromMaybe 10 mbLimit
      offsetVal = fromIntegral $ fromMaybe 0 mbOffset
  Esq.findAll $ do
    role <- from $ table @RoleT
    where_ $
      Esq.whenJust_ mbSearchString (filterBySearchString role)
    orderBy [asc $ role ^. RoleName]
    limit limitVal
    offset offsetVal
    pure role
  where
    filterBySearchString role searchStr = do
      let likeSearchStr = (%) ++. val searchStr ++. (%)
      role ^. RoleName
        `ilike` likeSearchStr
