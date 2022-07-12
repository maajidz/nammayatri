CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp.DriverDrivingLicense
(
    id character(36) COLLATE pg_catalog."default" NOT NULL,
    driver_id character varying(36) COLLATE pg_catalog."default" NOT NULL,
    driver_license_number character varying(255) COLLATE pg_catalog."default",
    driver_license_start timestamp with time zone NOT NULL,
    driver_license_status character varying(10) NOT NULL,
    driver_verification_status character varying(10) NOT NULL,
    driver_license_expiry timestamp with time zone NOT NULL,
    class_of_vehicle text[][] COLLATE pg_catalog."default",
    request_id character(36) COLLATE pg_catalog."default" NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
    ,CONSTRAINT  DriverDrivingLicense_driver_id_fkey FOREIGN KEY (driver_id) REFERENCES atlas_driver_offer_bpp.person(id)
);


-- data DriverDrivingLicense = DriverDrivingLicense {
--     id :: Id DriverDrivingLicense,
--     driverId :: Id Person,
--     driverLicenseNumber :: Maybe Text,
--     driverLicenseStart :: Maybe UTCTime,
--     driverLicenseStatus :: IdfyStatus,
--     driverVerificationStatus :: Maybe IdfyStatus,
--     driverLicenseExpiry :: Maybe UTCTime,
--     classOfVehicle :: [COV],
--     request_id :: Text,
--     createdAt :: UTCTime,
--     updatedAt :: UTCTime
-- }
--     deriving (Generic)