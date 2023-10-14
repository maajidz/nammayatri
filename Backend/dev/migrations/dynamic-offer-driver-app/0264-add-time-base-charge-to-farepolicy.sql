ALTER TABLE atlas_driver_offer_bpp.fare_parameters
ADD COLUMN IF NOT EXISTS time_based_charge double precision DEFAULT 0;
ALTER TABLE atlas_driver_offer_bpp.fare_policy_progressive_details
ADD COLUMN IF NOT EXISTS average_speed_of_vehicle int;

WITH suv_fp_ids AS (
    SELECT fare_policy_id
    FROM atlas_driver_offer_bpp.fare_product
    WHERE merchant_id = 'favorit0-0000-0000-0000-00000favorit'
    AND vehicle_variant = 'SUV'
)
UPDATE atlas_driver_offer_bpp.fare_policy_progressive_details SET average_speed_of_vehicle = 20
WHERE fare_policy_id IN (SELECT fare_policy_id FROM suv_fp_ids);

ALTER TABLE atlas_driver_offer_bpp.fare_policy_slabs_details_slab
ADD COLUMN IF NOT EXISTS average_speed_of_vehicle int;

WITH sedan_fp_ids AS (
    SELECT fare_policy_id
    FROM atlas_driver_offer_bpp.fare_product
    WHERE merchant_id = 'favorit0-0000-0000-0000-00000favorit'
    AND vehicle_variant = 'SEDAN'
)
UPDATE atlas_driver_offer_bpp.fare_policy_slabs_details_slab SET average_speed_of_vehicle = 20
WHERE fare_policy_id IN (SELECT fare_policy_id FROM sedan_fp_ids);