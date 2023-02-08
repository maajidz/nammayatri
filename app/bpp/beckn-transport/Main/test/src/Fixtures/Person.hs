module Fixtures.Person where

import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as Person
import EulerHS.Prelude
import qualified Fixtures.Time as Fixtures
import Kernel.Types.Id
import Kernel.Types.Version

defaultVersion :: Version
defaultVersion = Version 0 0 0

defaultMerchantId :: Id DM.Merchant
defaultMerchantId = Id "merchant1"

defaultDriver :: Person.Person
defaultDriver =
  Person.Person
    { id = Id "1",
      firstName = "Driver",
      middleName = Nothing,
      lastName = Just "Driverson",
      role = Person.DRIVER,
      gender = Person.UNKNOWN,
      identifierType = Person.EMAIL,
      email = Just "driverson@cool-drivers.com",
      mobileNumber = Nothing,
      mobileCountryCode = Nothing,
      passwordHash = Nothing,
      identifier = Nothing,
      rating = Nothing,
      isNew = True,
      merchantId = defaultMerchantId,
      deviceToken = Nothing,
      description = Nothing,
      createdAt = Fixtures.defaultTime,
      updatedAt = Fixtures.defaultTime,
      bundleVersion = Just defaultVersion,
      clientVersion = Just defaultVersion
    }

anotherDriver :: Person.Person
anotherDriver =
  defaultDriver
    { Person.id = Id "anotherDriver"
    }

defaultAdmin :: Person.Person
defaultAdmin =
  defaultDriver
    { Person.id = Id "admin",
      Person.firstName = "Admin",
      Person.lastName = Just "Adminson",
      Person.role = Person.ADMIN,
      Person.email = Just "adminson@cool-admins.com"
    }

anotherMerchantId :: Id DM.Merchant
anotherMerchantId = Id "anotherMerchantId"

anotherMerchantAdmin :: Person.Person
anotherMerchantAdmin =
  defaultAdmin
    { Person.id = Id "anotherMerchantAdmin",
      Person.merchantId = anotherMerchantId
    }
