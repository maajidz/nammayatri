module SuggestionUtils where

import Data.Map (Map, insert, update, lookup, member, delete, keys, isEmpty)
import Data.Tuple.Nested ((/\))
import Engineering.Helpers.Commons (clearTimer, getCurrentUTC, getNewIDWithTag, convertUTCtoISC)
import Data.Maybe
import Prelude
import Data.Array(singleton,catMaybes, any, sortWith, reverse, take, filter, (:), length, (!!), fromFoldable, toUnfoldable, snoc)
import Data.Ord (comparing)
import Screens.Types (LocationListItemState(..),SourceGeoHash, DestinationGeoHash,SuggestionsMap(..), Suggestions(..), Trip(..), LocationItemType(..))
import Helpers.Utils(getDistanceBwCordinates, getDifferenceBetweenDates, parseSourceHashArray, toString)
import Data.Int(toNumber)
import Storage (getValueToLocalStore, setValueToLocalStore, KeyStore(..))
import MerchantConfig.Types (SuggestedDestinationAndTripsConfig)
import Debug(spy)
addOrUpdateSuggestedDestination ::
  SourceGeoHash ->
  LocationListItemState ->
  SuggestionsMap ->
  SuggestedDestinationAndTripsConfig ->
  SuggestionsMap
addOrUpdateSuggestedDestination sourceGeohash destination suggestionsMap config=
  let
    updateSuggestions :: Suggestions -> Maybe Suggestions
    updateSuggestions suggestion = Just $ suggestion {destinationSuggestions = updateDestinations suggestion.destinationSuggestions} 

    updateDestinations ::  (Array LocationListItemState) -> Array LocationListItemState
    updateDestinations destinations = (updateDestination destinations)

    updateDestination :: Array LocationListItemState -> Array LocationListItemState
    updateDestination destinations =
      let
        updateExisting :: LocationListItemState -> LocationListItemState
        updateExisting existingDestination =
          if destination.placeId == existingDestination.placeId
          then existingDestination
                 { frequencyCount = Just $ (fromMaybe 0 existingDestination.frequencyCount) +  1
                 , recencyDate = Just $ (getCurrentUTC "")
                 , locationScore = Just $ calculateScore (toNumber ((fromMaybe 0 existingDestination.frequencyCount) +  1)) (getCurrentUTC "") config.frequencyWeight
                 }
          else existingDestination
                { locationScore = Just $ calculateScore (toNumber (fromMaybe 0 existingDestination.frequencyCount)) (fromMaybe (getCurrentUTC "") existingDestination.recencyDate) config.frequencyWeight }

        updatedDestinations = map updateExisting destinations
        destinationExists = any (\d -> d.placeId == destination.placeId) destinations
        sortedDestinations = sortDestinationsByScore updatedDestinations
      in
        if destinationExists
        then sortedDestinations
        else  (take (config.locationsToBeStored - 1) sortedDestinations) <> ( singleton destination{recencyDate = (Just $ (getCurrentUTC "")),
                                                     frequencyCount = Just 1,
                                                     locationScore = Just $ calculateScore (toNumber 1) (getCurrentUTC "") config.frequencyWeight,
                                                     prefixImageUrl = "ic_fav,https://assets.juspay.in/nammayatri/images/user/ic_fav.png", 
                                                     locationItemType = Just SUGGESTED_DESTINATIONS
                                                     }) 
  in
    if member sourceGeohash suggestionsMap
    then update updateSuggestions sourceGeohash suggestionsMap
    else insertSuggestionInMap sourceGeohash {destinationSuggestions:(singleton destination{recencyDate = (Just $ (getCurrentUTC "")),
                                                     frequencyCount = Just 1,
                                                     locationScore = Just $ calculateScore (toNumber 1) (getCurrentUTC "") config.frequencyWeight,
                                                     prefixImageUrl = "ic_fav,https://assets.juspay.in/nammayatri/images/user/ic_fav.png",
                                                     locationItemType = Just SUGGESTED_DESTINATIONS
                                                     }),
                                                     tripSuggestions : []} suggestionsMap config.geohashLimitForMap


addOrUpdateSuggestedTrips ::
  SourceGeoHash ->
  Trip ->
  SuggestionsMap ->
  SuggestedDestinationAndTripsConfig ->
  SuggestionsMap
addOrUpdateSuggestedTrips sourceGeohash trip suggestionsMap config=
  let
    updateSuggestions :: Suggestions -> Maybe Suggestions
    updateSuggestions suggestion = Just $ suggestion {tripSuggestions = updateTrips suggestion.tripSuggestions} 

    updateTrips ::  (Array Trip) -> Array Trip
    updateTrips trips = (updateTrip trips)

    updateTrip :: Array Trip -> Array Trip
    updateTrip trips =
      let
        updateExisting :: Trip -> Trip
        updateExisting existingDestination =
          if (getDistanceBwCordinates trip.sourceLat trip.sourceLong existingDestination.sourceLat existingDestination.sourceLong) < 0.011
          && (getDistanceBwCordinates trip.destLat trip.destLong existingDestination.destLat existingDestination.destLong) < 0.011
          then existingDestination
                 { frequencyCount = Just $ (fromMaybe 0 existingDestination.frequencyCount) +  1
                 , recencyDate = Just $ (getCurrentUTC "")
                 , locationScore = Just $ calculateScore (toNumber ((fromMaybe 0 existingDestination.frequencyCount) +  1)) (getCurrentUTC "") config.frequencyWeight
                 }
          else existingDestination
                { locationScore = Just $ calculateScore (toNumber (fromMaybe 0 existingDestination.frequencyCount)) (fromMaybe (getCurrentUTC "") existingDestination.recencyDate) config.frequencyWeight }

        updatedDestinations = map updateExisting trips
        destinationExists = any (\d -> (getDistanceBwCordinates trip.sourceLat trip.sourceLong d.sourceLat d.sourceLong) < 0.011
          && (getDistanceBwCordinates trip.destLat trip.destLong d.destLat d.destLong) < 0.011) trips
        sortedDestinations = sortTripsByScore updatedDestinations
      in
        if destinationExists
        then sortedDestinations
        else  (take (config.tripsToBeStored - 1) sortedDestinations) <> ( singleton trip{recencyDate = (Just $ (getCurrentUTC "")),
                                                     frequencyCount = Just 1,
                                                     locationScore = Just $ calculateScore (toNumber 1) (getCurrentUTC "") config.frequencyWeight
                                                     }) 
  in
    if member sourceGeohash suggestionsMap
    then update updateSuggestions sourceGeohash suggestionsMap
    else insertSuggestionInMap sourceGeohash ({destinationSuggestions:[],
                              tripSuggestions : (singleton trip{recencyDate = (Just $ (getCurrentUTC "")),
                                                     frequencyCount = Just 1,
                                                     locationScore = Just $ calculateScore (toNumber 1) (getCurrentUTC "") config.frequencyWeight
                                                     })}) suggestionsMap config.geohashLimitForMap


getSuggestedRidesAndLocations :: SourceGeoHash -> SuggestionsMap -> Int ->Maybe Suggestions
getSuggestedRidesAndLocations sourceGeohash suggestionsMap geoHashLimit = do
  if (member sourceGeohash suggestionsMap && not (getValueToLocalStore SOURCE_GEOHASHES == "__failed" || getValueToLocalStore SOURCE_GEOHASHES == "(null)")) then do
    sourceHashList <- pure $ getValueToLocalStore SOURCE_GEOHASHES
    parsedHashList <- pure $ parseSourceHashArray sourceHashList
    sourceHashListInString <- pure $ toString (updateSourceGeohash sourceGeohash (take geoHashLimit parsedHashList))
    _ <- pure $ setValueToLocalStore SOURCE_GEOHASHES sourceHashListInString
    pure unit
  else do pure unit
  lookup sourceGeohash suggestionsMap

updateSourceGeohash :: SourceGeoHash -> Array SourceGeoHash -> Array SourceGeoHash
updateSourceGeohash sourceHash hashList = sourceHash : (filter (\hash -> hash /= sourceHash) hashList)

calculateScore :: Number -> String -> Number -> Number
calculateScore frequency recencyDate frequencyConfig =
  let
    frequencyWeight = frequencyConfig
    recencyWeight = 1.0 - frequencyWeight
    currentDate = (getCurrentUTC "")
    recencyInSeconds = getDifferenceBetweenDates currentDate recencyDate
    normalizedFrequency = frequency / (frequency + 1.0)

    normalizedRecency = 1.0 - (toNumber $ (recencyInSeconds / recencyInSeconds + 1))

    score = (frequencyWeight * normalizedFrequency) + (recencyWeight * normalizedRecency)
  in
    score


insertSuggestionInMap :: SourceGeoHash -> Suggestions -> SuggestionsMap -> Int -> SuggestionsMap
insertSuggestionInMap sourceGeohash suggestionItem suggestionsMap geohashLimit = 
  let hashList =  getValueToLocalStore SOURCE_GEOHASHES
      parsedHashList = parseSourceHashArray if (hashList == "__failed" || hashList == "(null)") then "[]" else hashList
      toDelete = length parsedHashList > (geohashLimit - 1)
      updatedHashList = if toDelete then snoc (take (geohashLimit - 1) parsedHashList) sourceGeohash else snoc parsedHashList sourceGeohash
      updatedMap = if toDelete then delete (fromMaybe "" (parsedHashList !! (geohashLimit - 1))) suggestionsMap else suggestionsMap
      _ = setValueToLocalStore SOURCE_GEOHASHES (toString updatedHashList)

    in insert sourceGeohash suggestionItem updatedMap


sortDestinationsByScore :: Array LocationListItemState -> Array LocationListItemState
sortDestinationsByScore destinations = reverse (sortWith (\d -> fromMaybe 0.0 d.locationScore) destinations)

sortTripsByScore :: Array Trip -> Array Trip
sortTripsByScore trips = reverse (sortWith (\d -> fromMaybe 0.0 d.locationScore) trips)

fetchKeys :: forall k v. Map k v -> Array k
fetchKeys = fromFoldable <<< keys

addKeys :: SuggestionsMap -> Array SourceGeoHash
addKeys = fetchKeys

