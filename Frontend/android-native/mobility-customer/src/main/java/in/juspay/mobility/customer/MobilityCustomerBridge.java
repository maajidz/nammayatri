package in.juspay.mobility.customer;

import android.Manifest;
import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.animation.ValueAnimator;
import android.annotation.SuppressLint;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.BroadcastReceiver;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.graphics.Color;
import android.graphics.Typeface;
import android.graphics.pdf.PdfDocument;
import android.location.Location;
import android.location.LocationManager;
import android.media.AudioAttributes;
import android.media.RingtoneManager;
import android.net.Uri;
import android.os.Build;
import android.os.Environment;
import android.os.Handler;
import android.os.Looper;
import android.provider.ContactsContract;
import android.util.Base64;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.animation.LinearInterpolator;
import android.webkit.JavascriptInterface;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.core.app.ActivityCompat;
import androidx.core.app.NotificationCompat;
import androidx.core.app.NotificationManagerCompat;
import androidx.core.content.ContextCompat;
import androidx.core.content.FileProvider;

import com.google.android.gms.maps.CameraUpdateFactory;
import com.google.android.gms.maps.model.BitmapDescriptorFactory;
import com.google.android.gms.maps.model.ButtCap;
import com.google.android.gms.maps.model.Dash;
import com.google.android.gms.maps.model.Gap;
import com.google.android.gms.maps.model.LatLng;
import com.google.android.gms.maps.model.Marker;
import com.google.android.gms.maps.model.MarkerOptions;
import com.google.android.gms.maps.model.PatternItem;
import com.google.maps.android.PolyUtil;
import com.google.maps.android.SphericalUtil;
import com.google.maps.android.data.geojson.GeoJsonLayer;
import com.google.maps.android.data.geojson.GeoJsonPolygonStyle;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

import in.juspay.hyper.core.BridgeComponents;
import in.juspay.hyper.core.ExecutorManager;
import in.juspay.hyper.core.JuspayLogger;
import in.juspay.mobility.app.CallBack;
import in.juspay.mobility.app.NetworkBroadcastReceiver;
import in.juspay.mobility.app.NotificationUtils;
import in.juspay.mobility.common.MobilityCommonBridge;

public class MobilityCustomerBridge extends MobilityCommonBridge {

    public static int debounceAnimateCameraCounter = 0;

    // CallBacks Strings
    private static String storeContactsCallBack = null;
    private static String storeCustomerCallBack = null;

    public MobilityCustomerBridge(BridgeComponents bridgeComponents) {
        super(bridgeComponents);
        if (isClassAvailable("in.juspay.mobility.app.CallBack")) {
            CallBack callBack = new CallBack() {
                @Override
                public void customerCallBack(String notificationType) {
                    callingStoreCallCustomer(notificationType);
                }

                @Override
                public void driverCallBack(String notificationType) {
                    Log.i(CALLBACK, "No Required");
                }

                @Override
                public void imageUploadCallBack(String encImage, String filename) {
                    Log.i(CALLBACK, "No Required");
                }

                @Override
                public void internetCallBack(String isPermission) {
                    callInternetActionCallBack(isPermission);
                }

                @Override
                public void chatCallBack(String message, String sentBy, String time) {
                    Log.i(CALLBACK, "No Required");
                }
            };
            NotificationUtils.registerCallback(callBack);
            NetworkBroadcastReceiver.registerCallback(callBack);
        }
    }


    //region Store and Trigger CallBack
    @JavascriptInterface
    public void contactPermission() {
        try {
            if (ContextCompat.checkSelfPermission(bridgeComponents.getContext(), Manifest.permission.READ_CONTACTS) != PackageManager.PERMISSION_GRANTED) {
                if (bridgeComponents.getActivity() != null) {
                    ActivityCompat.requestPermissions(bridgeComponents.getActivity(), new String[]{Manifest.permission.READ_CONTACTS}, REQUEST_CONTACTS);
                }
            } else {
                contactsStoreCall(getPhoneContacts());
            }
        } catch (Exception e) {
            Log.e(UTILS, "Exception in Contact Permission" + e);
        }
    }

    public void contactsStoreCall(String contacts) {
        if (storeContactsCallBack != null) {
            String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                    storeContactsCallBack, contacts);
            bridgeComponents.getJsCallback().addJsToWebView(javascript);
        }
    }

    @JavascriptInterface
    public void storeCallBackContacts(String callback) {
        storeContactsCallBack = callback;
    }
    // endregion

    @JavascriptInterface
    public void storeCallBackCustomer(String callback) {
        storeCustomerCallBack = callback;
    }

    public void callingStoreCallCustomer(String notificationType) {
        String javascript = String.format(Locale.ENGLISH, "window.callUICallback('%s','%s');",
                storeCustomerCallBack, notificationType);
        bridgeComponents.getJsCallback().addJsToWebView(javascript);
    }

    @JavascriptInterface
    public void storeCallBackLocateOnMap(String callback) {
        storeLocateOnMapCallBack = callback;
    }

    //region Maps
    @JavascriptInterface
    public String isCoordOnPath(String json, double currLat, double currLng, int speed) throws JSONException {
        LatLng currPoint = new LatLng(currLat, currLng);
        ArrayList<LatLng> path = new ArrayList<>();
        JSONObject jsonObject = new JSONObject(json);
        int distanceRemaining;
        JSONArray coordinates = jsonObject.getJSONArray("points");
        int eta;
        int resultIndex;
        JSONObject result = new JSONObject();
        for (int i = coordinates.length() - 1; i >= 0; i--) {
            JSONObject coordinate = (JSONObject) coordinates.get(i);
            double lng = coordinate.getDouble("lng");
            double lat = coordinate.getDouble("lat");
            LatLng tempPoints = new LatLng(lat, lng);
            path.add(tempPoints);
        }
        if (path.size() == 0) {
            result.put("points", new JSONArray());
            result.put("eta", 0);
            result.put("distance", 0);
            result.put("isInPath", false);
            return result.toString();
        }
        double locationOnPathThres = Double.parseDouble(getKeysInSharedPref("ACCURACY_THRESHOLD").equals("__failed") ? "30.0" : getKeysInSharedPref("ACCURACY_THRESHOLD"));
        resultIndex = PolyUtil.locationIndexOnEdgeOrPath(currPoint, path, PolyUtil.isClosedPolygon(path), true, locationOnPathThres);
        if (resultIndex == -1) {
            result.put("points", coordinates);
            result.put("eta", 0);
            result.put("distance", 0);
            result.put("isInPath", false);
        } else if (resultIndex == 0) {
            path.clear();
            result.put("points", new JSONArray());
            result.put("eta", 0);
            result.put("distance", 0);
            result.put("isInPath", true);
        } else if (resultIndex == (path.size() - 2) || resultIndex == (path.size() - 1)) {
            distanceRemaining = (int) SphericalUtil.computeLength(path);
            eta = distanceRemaining / speed;
            result.put("points", coordinates);
            result.put("eta", eta);
            result.put("distance", distanceRemaining);
            result.put("isInPath", true);
        } else {
            path.subList(resultIndex + 2, path.size()).clear();
            distanceRemaining = (int) SphericalUtil.computeLength(path);
            eta = distanceRemaining / speed;
            JSONArray remainingPoints = new JSONArray();
            for (int i = path.size() - 1; i >= 0; i--) {
                LatLng point = path.get(i);
                JSONObject tempPoints = new JSONObject();
                tempPoints.put("lat", point.latitude);
                tempPoints.put("lng", point.longitude);
                remainingPoints.put(tempPoints);
            }
            result.put("points", remainingPoints);
            result.put("eta", eta);
            result.put("distance", distanceRemaining);
            result.put("isInPath", true);
        }
        return result.toString();
    }

    @JavascriptInterface
    public void exitLocateOnMap(String str) {
        try {
            storeLocateOnMapCallBack = null;
            ExecutorManager.runOnMainThread(() -> {
                if (googleMap != null) {
                    googleMap.setOnCameraMoveListener(null);
                    googleMap.setOnCameraIdleListener(null);
                }
                for (Marker m : pickupPointsZoneMarkers) {
                    m.setVisible(false);
                }

                if (layer != null) {
                    layer.removeLayerFromMap();
                }
            });
        } catch (Exception e) {
            Log.i(MAPS, "LocateOnMap Exit Error for ", e);
        }
    }

    @JavascriptInterface
    public void locateOnMap(boolean goToCurrentLocation, final String lat, final String lon) {
        try {
            ExecutorManager.runOnMainThread(() -> {
                removeMarker("ny_ic_customer_current_location");
                final LatLng position = new LatLng(Double.parseDouble(lat), Double.parseDouble(lon));
                if (goToCurrentLocation) {
                    LatLng latLng = new LatLng(lastLatitudeValue, lastLongitudeValue);
                    googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLng, 17.0f));
                } else {
                    googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(position, 17.0f));
                    googleMap.moveCamera(CameraUpdateFactory.zoomTo(googleMap.getCameraPosition().zoom + 2.0f));
                }
                googleMap.setOnCameraIdleListener(() -> {
                    double lat1 = (googleMap.getCameraPosition().target.latitude);
                    double lng = (googleMap.getCameraPosition().target.longitude);
                    if (storeLocateOnMapCallBack != null) {
                        String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", storeLocateOnMapCallBack, "LatLon", lat1, lng);
                        bridgeComponents.getJsCallback().addJsToWebView(javascript);
                    }
                });
                if ((lastLatitudeValue != 0.0 && lastLongitudeValue != 0.0) && goToCurrentLocation) {
                    LatLng latLngObjMain = new LatLng(lastLatitudeValue, lastLongitudeValue);
                    if (googleMap != null)
                        googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLngObjMain, 17.0f));
                } else {
                    if (googleMap != null) {
                        googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(position, 17.0f));
                        googleMap.moveCamera(CameraUpdateFactory.zoomTo(googleMap.getCameraPosition().zoom + 2.0f));
                    }
                }
            });

        } catch (Exception e) {
            Log.i(MAPS, "LocateOnMap error for ", e);
        }
    }

    @JavascriptInterface
    public void updateRoute(String json, String dest, String eta) {
        ExecutorManager.runOnMainThread(() -> {
            if (googleMap != null) {
                try {
                    ArrayList<LatLng> path = new ArrayList<>();
                    JSONObject jsonObject = new JSONObject(json);
                    JSONArray coordinates = jsonObject.getJSONArray("points");
                    for (int i = coordinates.length() - 1; i >= 0; i--) {
                        JSONObject coordinate = (JSONObject) coordinates.get(i);
                        double lng = coordinate.getDouble("lng");
                        double lat = coordinate.getDouble("lat");
                        LatLng tempPoint = new LatLng(lat, lng);
                        path.add(tempPoint);
                    }
                    Marker currMarker = (Marker) markers.get("ny_ic_vehicle_nav_on_map");
                    Marker destMarker = (Marker) markers.get(dest);
                    destMarker.setIcon((BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(eta, dest))));
                    if (polyline != null) {
                        polyline.setEndCap(new ButtCap());
                        if (path.size() == 0) {
                            LatLng destination = destMarker.getPosition();
                            animateMarkerNew(destination, currMarker);
                            polyline.remove();
                            polyline = null;
                            currMarker.setAnchor(0.5f, 0);
                            animateCamera(destMarker.getPosition().latitude, destMarker.getPosition().longitude, 17.0f);
                        } else {
                            double destinationLat = path.get(0).latitude;
                            double destinationLon = path.get(0).longitude;
                            double sourceLat = path.get(path.size() - 1).latitude;
                            double sourceLong = path.get(path.size() - 1).longitude;
                            LatLng destination = path.get(path.size() - 1);
                            animateMarkerNew(destination, currMarker);
                            PatternItem DASH = new Dash(1);
                            List<PatternItem> PATTERN_POLYLINE_DOTTED_DASHED = Collections.singletonList(DASH);
                            polyline.setPattern(PATTERN_POLYLINE_DOTTED_DASHED);
                            polyline.setPoints(path);
                            if (debounceAnimateCameraCounter != 0) {
                                debounceAnimateCameraCounter--;
                            } else {
                                moveCamera(sourceLat, sourceLong, destinationLat, destinationLon, coordinates);
                                debounceAnimateCameraCounter = 10;
                            }
                        }
                    }
                } catch (JSONException e) {
                    e.printStackTrace();
                }
            }
        });
    }

    private void animateMarkerNew(final LatLng destination, final Marker marker) {
        if (marker != null) {

            LatLng startPosition = marker.getPosition();


            ValueAnimator valueAnimator = ValueAnimator.ofFloat(0, 1);
            valueAnimator.setDuration(2000); // TODO :: get this value from Loacl Storage to maintain sync with PS
            valueAnimator.setInterpolator(new LinearInterpolator());
            valueAnimator.addUpdateListener(animation -> {
                try {
                    float v = animation.getAnimatedFraction();
                    LatLng newPosition = SphericalUtil.interpolate(startPosition, destination, v);
                    float rotation = bearingBetweenLocations(startPosition, destination);
                    if (rotation > 1.0)
                        marker.setRotation(rotation);
                    marker.setPosition(newPosition);
                    markers.put("ny_ic_vehicle_nav_on_map", marker);
                } catch (Exception e) {
                    e.printStackTrace();
                }
            });
            valueAnimator.addListener(new AnimatorListenerAdapter() {
                @Override
                public void onAnimationEnd(Animator animation) {
                    super.onAnimationEnd(animation);
                }
            });
            valueAnimator.start();
        }
    }

    public JSONObject getNearestPoint(double lat, double lng, JSONArray path) throws JSONException {

        JSONObject jsonObject = new JSONObject();

        Location locationA = new Location("point A");
        locationA.setLatitude(lat);
        locationA.setLongitude(lng);

        double minDist = 10000000000.0;

        Location location = new Location("final point");

        for (int i = 0; i < path.length(); i++) {
            JSONObject a = path.getJSONObject(i);
            double px = (Double) a.get("lat");
            double py = (Double) a.get("lng");

            Location locationB = new Location("point B");
            locationB.setLatitude(px);
            locationB.setLongitude(py);

            float distance = locationA.distanceTo(locationB);

            if (distance < minDist) {
                minDist = distance;
                location = locationB;
                zoneName = a.getString("place");
            }
        }
        jsonObject.put("place", zoneName);
        jsonObject.put("lat", location.getLatitude());
        jsonObject.put("long", location.getLongitude());
        return jsonObject;
    }

    public void drawMarkers(double lat, double lng, String name) {
        ExecutorManager.runOnMainThread(() -> {
            try {
                MarkerOptions markerOptionsObj = new MarkerOptions()
                        .title("")
                        .position(new LatLng(lat, lng))
                        .anchor(0.49f, 0.78f)
                        .icon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(name, "ny_ic_zone_pickup_marker")));
                Marker m = googleMap.addMarker(markerOptionsObj);
                if (m != null) {
                    m.hideInfoWindow();
                    pickupPointsZoneMarkers.add(m);
                }
            } catch (Exception e) {
                Log.d("error on pickup markers", e.toString());
            }
        });
    }

    @JavascriptInterface
    public void drawPolygon(String geoJson, String locationName) throws JSONException {

        ExecutorManager.runOnMainThread(() -> {
            System.out.println("Inside drawPolygon function");
            if (layer != null) {
                layer.removeLayerFromMap();
            }
            JSONObject geoJsonLayer;
            try {
                geoJsonLayer = new JSONObject(geoJson);
                PatternItem DASH = new Dash(20);
                PatternItem GAP = new Gap(20);
                List<PatternItem> PATTERN_POLYGON_ALPHA = Arrays.asList(GAP, DASH);
                layer = new GeoJsonLayer(googleMap, geoJsonLayer);
                GeoJsonPolygonStyle polyStyle = layer.getDefaultPolygonStyle();
                polyStyle.setFillColor(Color.argb(25, 0, 102, 255));
                polyStyle.setStrokePattern(PATTERN_POLYGON_ALPHA);
                polyStyle.setStrokeWidth(2);
                polyStyle.setStrokeColor(Color.BLUE);
                if (locationName.length() > 0) {
                    zoom = 14.0f;
                    if (userPositionMarker == null) {
                        upsertMarker(CURRENT_LOCATION, String.valueOf(getKeyInNativeSharedPrefKeys("LAST_KNOWN_LAT")), String.valueOf(getKeyInNativeSharedPrefKeys("LAST_KNOWN_LON")), 160, 0.5f, 0.9f); //TODO this function will be removed
                    } else {
                    }
                    userPositionMarker.setIcon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView(locationName, CURRENT_LOCATION)));
                    userPositionMarker.setTitle("");
                }
                layer.addLayerToMap();
            } catch (JSONException e) {
                e.printStackTrace();
            }
        });
    }

    @JavascriptInterface
    public void removeLabelFromMarker() {
        ExecutorManager.runOnMainThread(() -> {
            try {
                zoom = 17.0f;
                if (layer != null) {
                    layer.removeLayerFromMap();
                }
                if (userPositionMarker != null) {
                    userPositionMarker.setIcon(BitmapDescriptorFactory.fromBitmap(getMarkerBitmapFromView("", CURRENT_LOCATION)));
                    userPositionMarker.setTitle("");
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    @JavascriptInterface
    public void locateOnMap(boolean goToCurrentLocation, final String lat, final String lon, String geoJson, String points) {
        System.out.println("Inside locateOnMap" + geoJson);
        if (geoJson.equals("")) {
            locateOnMap(goToCurrentLocation, lat, lon);
            return;
        }
        try {
            ExecutorManager.runOnMainThread(() -> {
                try {
                    drawPolygon(geoJson, "");
                } catch (JSONException e) {
                    e.printStackTrace();
                }
                removeMarker("ny_ic_customer_current_location");
                if (goToCurrentLocation) {
                    LatLng latLng = new LatLng(lastLatitudeValue, lastLongitudeValue);
                    googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLng, 17.0f));
                } else {
                    LatLng latLng = new LatLng(Double.parseDouble(lat), Double.parseDouble(lon));
                    googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLng, 17.0f));
                    googleMap.moveCamera(CameraUpdateFactory.zoomTo(googleMap.getCameraPosition().zoom + 2.0f));
                }
                googleMap.setOnCameraIdleListener(() -> {
                    double lat1 = (googleMap.getCameraPosition().target.latitude);
                    double lng = (googleMap.getCameraPosition().target.longitude);
                    System.out.println("Inside OnCameraIdle");
                    Handler handler = new Handler(Looper.getMainLooper());
                    System.out.println("Inside OnCameraIdle");
                    ExecutorManager.runOnBackgroundThread(() -> {
                        try {
                            new Thread(() -> {
                                Boolean res = isPointInside(lat1, lng);
                                System.out.println("Inside OnCameraIdle");
                                handler.post(() -> {
                                    try {
                                        if (res) {
                                            System.out.println("Inside OnCameraIdle" + res);
                                            JSONArray zonePoints = new JSONArray(points);
                                            System.out.println("Inside zonepoints" + zonePoints);
                                            JSONObject nearestPickupPointObj = getNearestPoint(lat1, lng, zonePoints);
                                            Location nearestPickupPoint = new Location("");
                                            nearestPickupPoint.setLatitude(nearestPickupPointObj.getDouble("lat"));
                                            nearestPickupPoint.setLongitude(nearestPickupPointObj.getDouble("long"));

                                            for (Marker m : pickupPointsZoneMarkers) {
                                                m.setVisible(false);
                                            }

                                            for (int i = 0; i < zonePoints.length(); i++) {
                                                if (SphericalUtil.computeDistanceBetween(googleMap.getCameraPosition().target, new LatLng((Double) zonePoints.getJSONObject(i).get("lat"), (Double) zonePoints.getJSONObject(i).get("lng"))) <= 1) {
                                                    drawMarkers((Double) zonePoints.getJSONObject(i).get("lat"), (Double) zonePoints.getJSONObject(i).get("lng"), (String) zonePoints.getJSONObject(i).get("place"));
                                                    zoneName = (String) zonePoints.getJSONObject(i).get("place");
                                                } else {
                                                    drawMarkers((Double) zonePoints.getJSONObject(i).get("lat"), (Double) zonePoints.getJSONObject(i).get("lng"), "");
                                                }
                                            }

                                            if (SphericalUtil.computeDistanceBetween(googleMap.getCameraPosition().target, new LatLng(nearestPickupPoint.getLatitude(), nearestPickupPoint.getLongitude())) > 1) {
                                                animateCamera(nearestPickupPoint.getLatitude(), nearestPickupPoint.getLongitude(), 25.0f);
                                            }
                                        } else {
                                            for (Marker m : pickupPointsZoneMarkers) {
                                                m.setVisible(false);
                                            }
                                        }
                                        if (storeLocateOnMapCallBack != null) {
                                            String javascript = String.format("window.callUICallback('%s','%s','%s','%s');", storeLocateOnMapCallBack, zoneName, lat1, lng);
                                            Log.e(MAPS, javascript);
                                            bridgeComponents.getJsCallback().addJsToWebView(javascript);
                                        }
                                    } catch (JSONException e) {
                                        System.out.println("Exception " + e);
                                    }
                                });
                            }).start();
                        } catch (Exception e) {
                            Log.e("api response error", e.toString());
                        }
                    });
                });
                if ((lastLatitudeValue != 0.0 && lastLongitudeValue != 0.0) && goToCurrentLocation) {
                    LatLng latLngObjMain = new LatLng(lastLatitudeValue, lastLongitudeValue);
                    googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLngObjMain, 17.0f));
                } else {
                    LatLng latLngObjMain = new LatLng(Double.parseDouble(lat), Double.parseDouble(lon));
                    googleMap.moveCamera(CameraUpdateFactory.newLatLngZoom(latLngObjMain, 17.0f));
                    googleMap.moveCamera(CameraUpdateFactory.zoomTo(googleMap.getCameraPosition().zoom + 2.0f));
                }
            });
        } catch (Exception e) {
            Log.i(MAPS, "LocateOnMap error for ", e);
        }
    }

    private Boolean isPointInside(Double lat, Double lng) {
        System.out.println("Inside isPOinteINside");
        StringBuilder result = new StringBuilder();
        regToken = getKeysInSharedPref("REGISTERATION_TOKEN");
        baseUrl = getKeysInSharedPref("BASE_URL");
        String version = getKeysInSharedPref("VERSION_NAME");
        System.out.println("BaseUrl" + baseUrl);
        try {
            String url = baseUrl + "/serviceability/origin";
            HttpURLConnection connection = (HttpURLConnection) (new URL(url).openConnection());
            connection.setRequestMethod("POST");
            connection.setRequestProperty("Content-Type", "application/json");
            connection.setRequestProperty("token", regToken);
            connection.setRequestProperty("x-client-version", version);

            JSONObject payload = new JSONObject();

            JSONObject latLng = new JSONObject();
            latLng.put("lat", lat);
            latLng.put("lon", lng);
            payload.put("location", latLng);

            OutputStream stream = connection.getOutputStream();
            stream.write(payload.toString().getBytes());
            connection.connect();
            int respCode = connection.getResponseCode();
            System.out.println("Response Code ::" + respCode);
            InputStreamReader respReader;
            if ((respCode < 200 || respCode >= 300) && respCode != 302) {
                respReader = new InputStreamReader(connection.getErrorStream());
                BufferedReader in = new BufferedReader(respReader);
                String inputLine;
                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
            } else {
                respReader = new InputStreamReader(connection.getInputStream());
                BufferedReader in = new BufferedReader(respReader);
                String inputLine;
                while ((inputLine = in.readLine()) != null) {
                    result.append(inputLine);
                }
                JSONObject res = new JSONObject(String.valueOf(result));
                System.out.println(res.getString("serviceable") + "my point result");
                return true;
            }

            return false;
        } catch (Exception e) {
            return false;
        }
    }
    //endregion

    private float bearingBetweenLocations(LatLng latLng1, LatLng latLng2) {
        double PI = 3.14159;
        double lat1 = latLng1.latitude * PI / 180;
        double long1 = latLng1.longitude * PI / 180;
        double lat2 = latLng2.latitude * PI / 180;
        double long2 = latLng2.longitude * PI / 180;
        double dLon = (long2 - long1);
        double y = Math.sin(dLon) * Math.cos(lat2);
        double x = Math.cos(lat1) * Math.sin(lat2) - Math.sin(lat1)
                * Math.cos(lat2) * Math.cos(dLon);
        double brng = Math.atan2(y, x);
        brng = Math.toDegrees(brng);
        brng = (brng + 360) % 360;
        return (float) brng;
    }

    @JavascriptInterface
    public String getExtendedPath(String json) throws JSONException {
        ArrayList<LatLng> path = new ArrayList<>();
        ArrayList<LatLng> extendedPath = new ArrayList<>();
        JSONObject jsonObject = new JSONObject(json);
        JSONArray coordinates = jsonObject.getJSONArray("points");
        if (coordinates.length() <= 1) return json;
        int pointsFactor = Integer.parseInt(getKeysInSharedPref("POINTS_FACTOR"));

        for (int i = coordinates.length() - 1; i >= 0; i--) {
            JSONObject coordinate = (JSONObject) coordinates.get(i);
            double lng = coordinate.getDouble("lng");
            double lat = coordinate.getDouble("lat");
            LatLng tempPoints = new LatLng(lat, lng);
            path.add(tempPoints);
        }
        for (int i = 0, j = 1; i < path.size() - 1 && j <= path.size() - 1; i++, j++) {
            LatLng point1 = path.get(i);
            LatLng point2 = path.get(j);
            extendedPath.add(point1);
            double distanceBtw = SphericalUtil.computeDistanceBetween(point1, point2);
            int noOfPoints = (int) Math.ceil(distanceBtw / pointsFactor);
            float fraction = 1.0f / (noOfPoints + 1);
            for (int k = 1; k <= noOfPoints; k++) {
                LatLng point = getNewLatLng(fraction * k, point1, point2);
                extendedPath.add(point);
            }
        }
        extendedPath.add(path.get(path.size() - 1));
        JSONObject newPoints = new JSONObject();
        JSONArray remainingPoints = new JSONArray();
        for (int i = extendedPath.size() - 1; i >= 0; i--) {
            LatLng point = extendedPath.get(i);
            JSONObject tempPoints = new JSONObject();
            tempPoints.put("lat", point.latitude);
            tempPoints.put("lng", point.longitude);
            remainingPoints.put(tempPoints);
        }
        newPoints.put("points", remainingPoints);
        return newPoints.toString();
    }

    private LatLng getNewLatLng(float fraction, LatLng a, LatLng b) {
        double lat = (b.latitude - a.latitude) * fraction + a.latitude;
        double lngDelta = b.longitude - a.longitude;
        // Take the shortest path across the 180th meridian.
        if (Math.abs(lngDelta) > 180) {
            lngDelta -= Math.signum(lngDelta) * 360;
        }
        double lng = lngDelta * fraction + a.longitude;
        return new LatLng(lat, lng);
    }

    @JavascriptInterface
    public void fetchAndUpdateCurrentLocation(String callback) {
        if (!isLocationPermissionEnabled()) return;
        updateLastKnownLocation(callback, true);
    }

    //region Others
    public String getPhoneContacts() throws JSONException {
        ContentResolver contentResolver = bridgeComponents.getContext().getContentResolver();
        Uri uri = ContactsContract.CommonDataKinds.Phone.CONTENT_URI;
        JSONArray contacts;
        try (Cursor cursor = contentResolver.query(uri, null, null, null, null)) {
            contacts = new JSONArray();

            if (cursor.getCount() > 0) {
                while (cursor.moveToNext()) {
                    String contactNameStr = cursor.getString(cursor.getColumnIndexOrThrow(ContactsContract.CommonDataKinds.Phone.DISPLAY_NAME));
                    String contactStr = cursor.getString(cursor.getColumnIndexOrThrow(ContactsContract.CommonDataKinds.Phone.NUMBER));
                    String contactNumber = contactStr.replaceAll("[^0-9]", "");
                    String contactName = contactNameStr.replaceAll("'", "");
                    JSONObject tempPoints = new JSONObject();
                    tempPoints.put("name", contactName);
                    tempPoints.put("number", contactNumber);
                    contacts.put(tempPoints);
                }
            }
        }

        JSONObject flagObject = new JSONObject();
        flagObject.put("name", "beckn_contacts_flag");
        flagObject.put("number", "true");
        contacts.put(flagObject);
        System.out.print("Contacts " + contacts);
        return contacts.toString();
    }

    @JavascriptInterface
    public void generatePDF(String str, String format) throws JSONException {
        invoice = str;
        if (isStoragePermissionGiven()) {
            JuspayLogger.d(OTHERS,"Storage Permission is already there");
            downloadPDF(str, bridgeComponents.getContext());
        } else {
            JuspayLogger.d(OTHERS,"Storage Permission is not there. requesting permissions");
            requestStoragePermission();
        }
    }

    @SuppressLint("MissingPermission")
    public void downloadPDF(String str, Context context) throws JSONException {
        new Thread(() -> {
            try {
                JSONObject state = new JSONObject(str);
                JSONObject data = state.getJSONObject("data");
                String userName = getKeysInSharedPref("USER_NAME");
                JSONObject selectedItem = data.getJSONObject("selectedItem");
                JSONArray fares = selectedItem.getJSONArray("faresList");
                PdfDocument pdfDocument = new PdfDocument();
                PdfDocument.PageInfo invoicePDF = new PdfDocument.PageInfo.Builder(960, 1338, 1).create();
                PdfDocument.Page page = pdfDocument.startPage(invoicePDF);
                JuspayLogger.d(OTHERS, "PDF Document Created");
                View content = getInvoiceLayout(selectedItem, fares, userName, context);
                JuspayLogger.d(OTHERS, "PDF Layout inflated");
                content.measure(page.getCanvas().getWidth(), page.getCanvas().getHeight());
                content.layout(0, 0, page.getCanvas().getWidth(), page.getCanvas().getHeight());
                content.draw(page.getCanvas());
                pdfDocument.finishPage(page);
                JuspayLogger.d(OTHERS, "PDF Document canvas drawn");
                String fileNameformat = "";
                String serviceName = context.getResources().getString(R.string.service);
                if (serviceName.equals("jatrisaathi")) {
                    fileNameformat = "JS_RIDE_";
                } else if (serviceName.equals("nammayatri")) {
                    fileNameformat = "NY_RIDE_";
                } else {
                    fileNameformat = "YATRI_RIDE_";
                }
                fileNameformat = fileNameformat + selectedItem.getString("date") + selectedItem.getString("rideStartTime");
                String removedSpecial = fileNameformat.replaceAll("[^a-zA-Z0-9]", "_");
                JuspayLogger.d(OTHERS, "PDF Document name " + removedSpecial);
                try {
                    File file = checkAndGetFileName(removedSpecial);
                    JuspayLogger.d(OTHERS, "Available File name for PDF" + file.getName());
                    FileOutputStream fos = new FileOutputStream(file);
                    pdfDocument.writeTo(fos);
                    JuspayLogger.d(OTHERS, "PDF Document written to path " + file.getPath());
                    Uri path = FileProvider.getUriForFile(context, context.getPackageName() + ".provider", file);
                    showInvoiceNotification(path);
                } catch (IOException e) {
                    e.printStackTrace();
                }
                pdfDocument.close();
                JuspayLogger.d(OTHERS, "PDF Document closed ");
            } catch (Exception e) {
                JuspayLogger.e(OTHERS,e.toString());
            }
        }).start();
    }

    private void showInvoiceNotification(Uri path) {
        Context context = bridgeComponents.getContext();
        JuspayLogger.d(OTHERS,"PDF Document inside Show Notification");
        Intent pdfOpenintent = new Intent(Intent.ACTION_VIEW);
        pdfOpenintent.setFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION | Intent.FLAG_ACTIVITY_CLEAR_TOP);
        pdfOpenintent.setDataAndType(path, "application/pdf");
        String CHANNEL_ID = "Invoice";
        PendingIntent pendingIntent = PendingIntent.getActivity(context, 234567, pdfOpenintent, PendingIntent.FLAG_IMMUTABLE);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            NotificationChannel channel = new NotificationChannel(CHANNEL_ID, "Invoice Download", NotificationManager.IMPORTANCE_HIGH);
            channel.setDescription("Invoice Download");
            NotificationManager notificationManager = context.getSystemService(NotificationManager.class);
            AudioAttributes attributes = new AudioAttributes.Builder()
                    .setContentType(AudioAttributes.CONTENT_TYPE_SONIFICATION)
                    .setUsage(AudioAttributes.USAGE_NOTIFICATION)
                    .build();
            channel.setSound(RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION),attributes);
            notificationManager.createNotificationChannel(channel);
        }
        NotificationCompat.Builder mBuilder = new NotificationCompat.Builder(context, CHANNEL_ID);
        int launcher = bridgeComponents.getContext().getResources().getIdentifier("ic_launcher", "mipmap", bridgeComponents.getContext().getPackageName());
        mBuilder.setContentTitle("Invoice Downloaded")
                .setSmallIcon(launcher)
                .setContentText("Invoice for your ride is downloaded!!!")
                .setAutoCancel(true)
                .setSound(RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION))
                .setPriority(NotificationCompat.PRIORITY_MAX);
        mBuilder.setContentIntent(pendingIntent);
        NotificationManagerCompat notificationManager = NotificationManagerCompat.from(context);
        JuspayLogger.d(OTHERS,"PDF Document notification is Created");
        if (ActivityCompat.checkSelfPermission(bridgeComponents.getContext(), Manifest.permission.POST_NOTIFICATIONS) != PackageManager.PERMISSION_GRANTED) {
            JuspayLogger.d(OTHERS,"PDF Document Notification permission is not given");
            toast("Invoice Downloaded!!!");
        } else {
            notificationManager.notify(234567, mBuilder.build());
            JuspayLogger.d(OTHERS,"PDF Document notification is notified");
        }
    }

    @SuppressLint("SetTextI18n")
    private View getInvoiceLayout(JSONObject selectedRide, JSONArray fares, String user, Context context) throws JSONException {
        JuspayLogger.d(OTHERS,"PDF Document inside inflate View");
        View invoiceLayout = LayoutInflater.from(context).inflate(R.layout.invoice_template, null, false);
        JuspayLogger.d(OTHERS,"PDF Document inflated View");
        TextView textView = invoiceLayout.findViewById(R.id.rideDate);
        textView.setText(selectedRide.getString("date"));
        textView = invoiceLayout.findViewById(R.id.userName);
        textView.setText(user.trim());
        textView = invoiceLayout.findViewById(R.id.paymentDetail);
        textView.setText(selectedRide.getString("totalAmount"));
        LinearLayout fareBreakupElements = invoiceLayout.findViewById(R.id.fareBreakupElements);
        fareBreakupElements.setOrientation(LinearLayout.VERTICAL);

        try {
            for (int i = 0; i < fares.length(); i++) {
                LinearLayout.LayoutParams linearParams = new LinearLayout.LayoutParams(
                        LinearLayout.LayoutParams.MATCH_PARENT,
                        LinearLayout.LayoutParams.WRAP_CONTENT);
                LinearLayout linearLayout = new LinearLayout(context);
                linearLayout.setLayoutParams(linearParams);

                LinearLayout.LayoutParams linearParamsChild = new LinearLayout.LayoutParams(
                        LinearLayout.LayoutParams.MATCH_PARENT,
                        LinearLayout.LayoutParams.WRAP_CONTENT);
                LinearLayout linearLayoutChild = new LinearLayout(context);
                linearLayoutChild.setLayoutParams(linearParamsChild);
                linearParamsChild.weight = 1.0f;

                JSONObject fare = fares.getJSONObject(i);
                JuspayLogger.d(OTHERS,"PDF Document updating fares break ups" + fare);
                String value = fare.getString("price");
                String fareTypes = fare.getString("title");
                TextView textViewText = new TextView(context);
                textViewText.setTextSize(5);
                textViewText.setTextColor(Color.parseColor("#454545"));
                textViewText.setPadding(0, 0, 0, 10);
                textViewText.setText(fareTypes);
                linearLayout.addView(textViewText);
                linearLayout.addView(linearLayoutChild);

                TextView textViewPrice = new TextView(context);
                textViewPrice.setTextSize(5);
                textViewPrice.setPadding(0, 0, 0, 10);
                textViewPrice.setTextColor(Color.parseColor("#454545"));
                textViewPrice.setText("₹ " + value);
                linearLayout.addView(textViewPrice);

                fareBreakupElements.addView(linearLayout);
                JuspayLogger.d(OTHERS,"PDF Document updated the fare " + fare + "in view");
            }
        } catch (JSONException e) {
            e.printStackTrace();
        }
        textView = invoiceLayout.findViewById(R.id.finalAmount);
        textView.setText(selectedRide.getString("totalAmount"));
        textView = invoiceLayout.findViewById(R.id.rideId);
        textView.setText(selectedRide.getString("shortRideId"));
        textView = invoiceLayout.findViewById(R.id.driverName);
        textView.setText(selectedRide.getString("driverName"));
        textView = invoiceLayout.findViewById(R.id.lincensePlate);
        textView.setText(selectedRide.getString("vehicleNumber"));
        textView = invoiceLayout.findViewById(R.id.rideStartTime);
        textView.setText(selectedRide.getString("rideStartTime"));
        textView = invoiceLayout.findViewById(R.id.source);
        textView.setText(selectedRide.getString("source"));
        textView = invoiceLayout.findViewById(R.id.rideEndTime);
        textView.setText(selectedRide.getString("rideEndTime"));
        textView = invoiceLayout.findViewById(R.id.destination);
        textView.setText(selectedRide.getString("destination"));
        textView = invoiceLayout.findViewById(R.id.referenceText);
        textView.setText(selectedRide.getString("referenceString"));
        JuspayLogger.d(OTHERS,"PDF Document view updated and returning the view");
        return invoiceLayout;
    }
    //endregion

    //region Override Functions
    @Override
    public boolean onActivityResult(int requestCode, int resultCode, Intent data) {

        return super.onActivityResult(requestCode, resultCode, data);
    }

    @Override
    public boolean onRequestPermissionResult(int requestCode, String[] permissions, int[] grantResults) {
        switch (requestCode) {
            case REQUEST_CALL:
                if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                    showDialer(phoneNumber);
                } else {
                    toast("Permission Denied");
                }
                break;
            case LOCATION_PERMISSION_REQ_CODE:
                if (grantResults.length > 0 && grantResults[0] != PackageManager.PERMISSION_GRANTED) {
                    toast("Permission Denied");
                }
                break;
            case STORAGE_PERMISSION:
                if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                    try {
                        JuspayLogger.d(OTHERS,"Storage Permission is granted. downloading  PDF");
                        downloadPDF(invoice, bridgeComponents.getContext());
                    } catch (JSONException e) {
                        e.printStackTrace();
                    }
                } else{
                    JuspayLogger.d(OTHERS,"Storage Permission is denied.");
                    toast("Permission Denied");
                }
                break;
            case REQUEST_CONTACTS:
                boolean flag = ContextCompat.checkSelfPermission(bridgeComponents.getContext(), Manifest.permission.READ_CONTACTS) == PackageManager.PERMISSION_GRANTED;
                String contacts;
                try {
                    if (flag) {
                        contacts = getPhoneContacts();
                    } else {
                        JSONArray flagArray = new JSONArray();
                        contacts = flagArray.toString();
                    }
                    contactsStoreCall(contacts);
                } catch (JSONException e) {
                    e.printStackTrace();
                }
                break;
            default:
                break;
        }
        return super.onRequestPermissionResult(requestCode, permissions, grantResults);
    }
    //endregion
}
