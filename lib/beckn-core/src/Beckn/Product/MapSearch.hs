{-# LANGUAGE OverloadedLabels #-}

module Beckn.Product.MapSearch where

import qualified Beckn.External.Graphhopper.Flow as Grphr
import qualified Beckn.External.Graphhopper.Types as Grphr
import qualified Beckn.Types.MapSearch as MapSearch
import Data.Geospatial
import qualified EulerHS.Language as L
import EulerHS.Prelude
import Servant
import Prelude (atan2)

getRoute :: MapSearch.Request -> L.Flow (Either SomeException (MapSearch.Response))
getRoute MapSearch.Request {..} = do
  -- Currently integrated only with graphhopper
  case all isLatLong waypoints of
    False -> return $ Left $ toException $ err400 {errBody = "Not supporting waypoints other than LatLong."}
    True -> do
      let points = map (\(MapSearch.LatLong point) -> point) waypoints
          mode' = fromMaybe MapSearch.CAR mode
          vehicle = mapToVehicle mode'
      res <- Grphr.search Grphr.defaultGrphrBaseUrl (grphrReq points vehicle)
      case res of
        Left err -> return $ Left $ toException err
        Right (Grphr.Response {..}) ->
          return $ Right $
            MapSearch.Response
              { status = "OK",
                routes = (mapToRoute mode') <$> _paths
              }
  where
    isLatLong :: MapSearch.MapPoint -> Bool
    isLatLong (MapSearch.LatLong _) = True
    isLatLong _ = False
    grphrReq :: [PointXY] -> Grphr.Vehicle -> Grphr.Request
    grphrReq points vehicle =
      Grphr.Request
        { _points' = points,
          _vehicle = vehicle,
          _weighting = Nothing,
          _elevation = Nothing,
          _calcPoints = calcPoints
        }

mapToVehicle :: MapSearch.TravelMode -> Grphr.Vehicle
mapToVehicle MapSearch.CAR = Grphr.CAR
mapToVehicle MapSearch.MOTORCYCLE = Grphr.SCOOTER
mapToVehicle MapSearch.BICYCLE = Grphr.BIKE
mapToVehicle MapSearch.FOOT = Grphr.FOOT

mapToRoute :: MapSearch.TravelMode -> Grphr.Path -> MapSearch.Route
mapToRoute mode Grphr.Path {..} =
  MapSearch.Route
    { distanceInM = _distance,
      durationInMS = _time,
      boundingBox = _bbox,
      snapped_waypoints = Just _snapped_waypoints,
      mode = mode,
      points = _points
    }

deg2Rad :: Double -> Double
deg2Rad degree = degree * pi / 180

distanceBetweenInMeters :: PointXY -> PointXY -> Float
distanceBetweenInMeters (PointXY lat1 lon1) (PointXY lat2 lon2) =
  -- Calculating using haversine formula
  let r = 6371000 -- Radius of earth in meters
      dlat = deg2Rad $ lat2 - lat1
      dlon = deg2Rad $ lon2 - lon1
      rlat1 = deg2Rad lat1
      rlat2 = deg2Rad lat2
      -- Calculated distance is real (not imaginary) when 0 <= h <= 1
      -- Ideally in our use case h wouldn't go out of bounds
      h = ((sin (dlat / 2)) ^ 2) + cos rlat1 * cos rlat2 * ((sin (dlon / 2)) ^ 2)
   in -- Float precision for distance is sufficient as we are working with `meter` units
      realToFrac $ 2 * r * atan2 (sqrt h) (sqrt (1 - h))

speedInMPS :: Float -> Integer -> Float
speedInMPS distance duration =
  if duration <= 0
    then 0 -- Realistically this is not possible, so just returning zero
    else distance / fromIntegral duration
