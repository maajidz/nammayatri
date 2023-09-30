ALTER TABLE atlas_driver_offer_bpp.ride ADD COLUMN safety_alert_trigger_count int DEFAULT 0;

ALTER TABLE atlas_driver_offer_bpp.rider_details ADD COLUMN night_safety_checks boolean NOT NULL default true;