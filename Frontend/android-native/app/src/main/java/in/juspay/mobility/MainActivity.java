/*
 *  Copyright 2022-23, Juspay India Pvt Ltd
 *  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program
 *  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 *  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of
 *  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
 */

package in.juspay.mobility;

import static in.juspay.mobility.BuildConfig.MERCHANT_TYPE;

import android.Manifest;
import android.animation.Animator;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.AlertDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.IntentSender;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.location.LocationManager;
import android.media.Ringtone;
import android.media.RingtoneManager;
import android.net.ConnectivityManager;
import android.net.Network;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.provider.Settings;
import android.system.Os;
import android.util.Base64;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowManager;
import android.view.animation.AnimationUtils;
import android.webkit.WebView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.cardview.widget.CardView;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.core.app.ActivityCompat;
import androidx.work.WorkManager;

import com.airbnb.lottie.LottieAnimationView;
import com.google.android.gms.maps.MapsInitializer;
import com.google.android.gms.tasks.Task;
import com.google.android.play.core.appupdate.AppUpdateInfo;
import com.google.android.play.core.appupdate.AppUpdateManager;
import com.google.android.play.core.appupdate.AppUpdateManagerFactory;
import com.google.android.play.core.install.model.AppUpdateType;
import com.google.android.play.core.install.model.UpdateAvailability;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.google.firebase.dynamiclinks.FirebaseDynamicLinks;
import com.google.firebase.messaging.FirebaseMessaging;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.ObjectOutputStream;
import java.util.Date;
import java.util.Timer;
import java.util.TimerTask;
import java.util.UUID;
import java.util.concurrent.TimeUnit;

import in.juspay.hypersdk.core.PaymentConstants;
import in.juspay.hypersdk.data.JuspayResponseHandler;
import in.juspay.hypersdk.ui.HyperPaymentsCallbackAdapter;
import in.juspay.mobility.app.BootUpReceiver;
import in.juspay.mobility.app.LocationUpdateService;
import in.juspay.mobility.app.MyFirebaseMessagingService;
import in.juspay.mobility.app.NetworkBroadcastReceiver;
import in.juspay.mobility.app.NotificationUtils;
import in.juspay.mobility.app.RideRequestActivity;
import in.juspay.mobility.app.Utils;
import in.juspay.mobility.app.WidgetService;
import in.juspay.services.HyperServices;


public class MainActivity extends AppCompatActivity {

    private static final String LOG_TAG = "MAIN_ACTIVITY";
    private static final int REQUEST_CODE_UPDATE_APP = 587;
    private static int updateType;
    @SuppressLint("StaticFieldLeak")
    NetworkBroadcastReceiver.ProcessCallBack processCallBack;
    MyFirebaseMessagingService.BundleUpdateCallBack bundleUpdateCallBack;
    private HyperServices hyperServices;
    private Context context;
    private Activity activity;
    @Nullable
    private SharedPreferences sharedPref;
    SharedPreferences.OnSharedPreferenceChangeListener mListener = new SharedPreferences.OnSharedPreferenceChangeListener() {
        @Override
        public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
            if (key != null && key.equals("REGISTERATION_TOKEN")) {
                String token = sharedPref.getString("REGISTERATION_TOKEN", "null");
                if (token.equals("__failed")) {
                    final PackageManager pm = getApplicationContext().getPackageManager();
                    final Intent intent = pm.getLaunchIntentForPackage(getApplicationContext().getPackageName());
                    try {
                        if (activity != null) {
                            activity.finishAffinity();// Finishes all activities.
                            activity.startActivity(intent);
                        } else {
                            sharedPref.edit().clear().apply();
                        }
                    } catch (NullPointerException e) {
                        e.printStackTrace();
                    }
                }
            }
            // Update Driver status in Local Storage
            if (key != null && key.equals("DRIVER_STATUS")) {
                String status = sharedPref.getString("DRIVER_STATUS", "null");
                WorkManager mWorkManager = WorkManager.getInstance(getApplicationContext());
                if (status.equals("null")) {
                    if (context != null) {
                        Intent locationUpdateIntent = new Intent(context, LocationUpdateService.class);
                        context.stopService(locationUpdateIntent);
                        mWorkManager.cancelAllWorkByTag(context.getString(in.juspay.mobility.app.R.string.location_update));
                    } else {
                        Context context = getApplicationContext();
                        Intent locationUpdateIntent = new Intent(context, LocationUpdateService.class);
                        context.stopService(locationUpdateIntent);
                        mWorkManager.cancelAllWorkByTag(context.getString(in.juspay.mobility.app.R.string.location_update));
                    }

                }
            }
            if (key != null && sharedPref.getString("DRIVER_STATUS", "null").equals("true") && (key.equals("RIDE_G_FREQUENCY") || key.equals("MAX_LIMIT_TO_STORE_LOCATION_PT") || key.equals("NO_OF_LOCATION_PT_TO_REMOVE") || key.equals("DRIVER_MIN_DISPLACEMENT") || key.equals("RIDE_T_FREQUENCY"))) {
                System.out.println("TRIGGERED UPDATE POLLING");
                Context context = getApplicationContext();
                Intent locationUpdateIntent = new Intent(context, LocationUpdateService.class);
                context.startService(locationUpdateIntent);
            }
        }
    };
    BroadcastReceiver gpsReceiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            LocationManager locationManager = (LocationManager) context.getSystemService(Context.LOCATION_SERVICE);
            boolean isGpsEnabled = locationManager.isProviderEnabled(LocationManager.GPS_PROVIDER);
            String token = "null";
            if (sharedPref != null) {
                token = sharedPref.getString("REGISTERATION_TOKEN", "null");
            }
            if (!isGpsEnabled && !token.equals("null")) {
                triggerPopUPMain("true", "LOCATION_DISABLED");
            }
        }
    };
    private Intent widgetService;
    private AppUpdateManager appUpdateManager;
    private NetworkBroadcastReceiver networkBroadcastReceiver;
    private boolean isHideSplashEventCalled = false;
    private boolean isSystemAnimEnabled = true;

    @Override
    public void onBackPressed() {
        if (hyperServices != null && !hyperServices.onBackPressed()) {
            super.onBackPressed();
        }
    }

    public String getDeviceRAM()
    {
        String deviceRAM = sharedPref.getString("DEVICE_RAM", "__failed");
        if(deviceRAM != "__failed")
            return deviceRAM;
        long memory=0;
        try {
            ActivityManager activityManager = (ActivityManager) getApplicationContext().getSystemService(Context.ACTIVITY_SERVICE);
            ActivityManager.MemoryInfo memInfo = new ActivityManager.MemoryInfo();
            activityManager.getMemoryInfo(memInfo);
            memory = 1 + memInfo.totalMem / (1024 * 1024 * 1024);
            deviceRAM = memory == 0 ? "null" : memory+" GB" ;
            sharedPref.edit().putString("DEVICE_RAM", deviceRAM).apply();
        } catch(Exception e){
            System.out.println("In getDeviceRAM error: ");
            e.printStackTrace();
        }
        return deviceRAM;
    }
    public String[] getScreenDimensions()
    {
        String[] res= {sharedPref.getString("DEVICE_RESOLUTION", "__failed"),sharedPref.getString("DEVICE_SIZE", "__failed")};
        if(res[0] != "__failed" && res[1] != "__failed")
            return res;
        int height = 0;
        int width  = 0;
        float size = 0;
        try {
            DisplayMetrics displayMetrics = new DisplayMetrics();
            getWindowManager().getDefaultDisplay().getRealMetrics(displayMetrics);
            height = displayMetrics.heightPixels;
            width = displayMetrics.widthPixels;
            float x = height / displayMetrics.ydpi;
            float y = width / displayMetrics.xdpi;
            size = (float) Math.sqrt(x * x + y * y);
            size = Math.round(size * 100) / 100;
            res[0] = height != 0 && width != 0 ? height+"x"+width+"px" : "null" ;
            res[1] = size!=0 ? size + " Inches" : "null" ;
            sharedPref.edit().putString("DEVICE_RESOLUTION", res[0]).apply();
            sharedPref.edit().putString("DEVICE_SIZE", res[1]).apply();
        }catch(Exception e){
            System.out.println("In getScreenDimensions error: ");
            e.printStackTrace();
        }
        return res;
    }

    public String getDeviceDetails()
    {
        String deviceDetails = "";
        try {
            String bVersion = Build.VERSION.RELEASE;
            String bModel = Build.MODEL;
            String bBrand = Build.BRAND;
            String[] dim = getScreenDimensions();
            String deviceRAM = getDeviceRAM();
            if(bModel == null || bModel == "")
                bModel="null";
            if(bBrand == null || bBrand == "")
                bBrand="null";
            bVersion = bVersion == null || bVersion == "" ? "null" : "Android v"+bVersion ;
            deviceDetails = bBrand+"/" + bModel+"/" + bVersion+"/" + deviceRAM + "/" + dim[1]+"/" + dim[0];
        } catch (Exception e) {
            e.printStackTrace();
        }
        return deviceDetails;
    }

    @SuppressLint("SetJavaScriptEnabled")
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        FirebaseAnalytics mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);
        context = getApplicationContext();
        try {
            MapsInitializer.initialize(getApplicationContext());
        } catch (Exception e) {
            e.printStackTrace();
        }
        activity = this;
        sharedPref = getSharedPreferences(this.getString(in.juspay.mobility.app.R.string.preference_file_key), Context.MODE_PRIVATE);
        sharedPref.edit().putString("DEVICE_DETAILS", getDeviceDetails()).apply();
        sharedPref.registerOnSharedPreferenceChangeListener(mListener);
        sharedPref.edit().putString(getResources().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onCreate").apply();
        @SuppressLint("HardwareIds") String androidId = Settings.Secure.getString(getContentResolver(), Settings.Secure.ANDROID_ID);
        Bundle params = new Bundle();
        params.putString("id", androidId);
        mFirebaseAnalytics.logEvent("device_id", params);
        widgetService = new Intent(this, WidgetService.class);
        FirebaseDynamicLinks.getInstance()
                .getDynamicLink(getIntent())
                .addOnSuccessListener(this, pendingDynamicLinkData -> {
                    // Get deep link from result (may be null if no link is found)
                    if (pendingDynamicLinkData != null) {
                        pendingDynamicLinkData.getLink();
                    }
                })
                .addOnFailureListener(this, e -> Log.w(LOG_TAG, "getDynamicLink:onFailure", e));


        WebView.setWebContentsDebuggingEnabled(true);
        setContentView(R.layout.activity_main);
        if (MERCHANT_TYPE.equals("DRIVER")) {
            getWindow().addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
            new Utils(context).updateLocaleResource(sharedPref.getString(getResources().getString(in.juspay.mobility.app.R.string.LANGUAGE_KEY), "null"));
        } else {
            LottieAnimationView splashLottieView = findViewById(in.juspay.mobility.app.R.id.splash_lottie);
            try {
                if (Settings.Global.getFloat(getContentResolver(), Settings.Global.ANIMATOR_DURATION_SCALE) == 0f) {
                    isSystemAnimEnabled = false;
                } else {
                    splashLottieView.addAnimatorListener(new Animator.AnimatorListener() {
                        @Override
                        public void onAnimationStart(Animator animation) {

                        }

                        @Override
                        public void onAnimationEnd(Animator animation) {
                            if (isHideSplashEventCalled) {
                                hideSplash();
                            } else {
                                splashLottieView.playAnimation();
                            }
                        }

                        @Override
                        public void onAnimationCancel(Animator animation) {
                        }

                        @Override
                        public void onAnimationRepeat(Animator animation) {

                        }
                    });
                }
            } catch (Settings.SettingNotFoundException e) {
                isSystemAnimEnabled = false;
            }
        }
        processCallBack = this::triggerPopUPMain;
        NetworkBroadcastReceiver.registerProcessCallback(processCallBack);
        bundleUpdateCallBack = this::showAlertForUpdate;
        MyFirebaseMessagingService.registerBundleUpdateCallback(bundleUpdateCallBack);
        appUpdateManager = AppUpdateManagerFactory.create(this);
        // Returns an intent object that you use to check for an update.
        Task<AppUpdateInfo> appUpdateInfoTask = appUpdateManager.getAppUpdateInfo();
        updateType = AppUpdateType.IMMEDIATE;
        appUpdateInfoTask.addOnSuccessListener(appUpdateInfo -> {
            if (appUpdateInfo.updateAvailability() == UpdateAvailability.UPDATE_AVAILABLE
                    && appUpdateInfo.isUpdateTypeAllowed(updateType)) {
                Log.d(LOG_TAG, "Inside update");
                try {
                    appUpdateManager.startUpdateFlowForResult(
                            // Pass the intent that is returned by 'getAppUpdateInfo()'.
                            appUpdateInfo,
                            // Or 'AppUpdateType.FLEXIBLE' for flexible updates.
                            updateType,
                            // The current activity making the update request.
                            this,
                            // Include a request code to later monitor this update request.
                            getResources().getInteger(REQUEST_CODE_UPDATE_APP)
                    );
                } catch (IntentSender.SendIntentException e) {
                    e.printStackTrace();
                }
                Log.d(LOG_TAG, "Update available");
            } else {
                Log.d(LOG_TAG, "No Update available");
            }
        });
        updateConfigURL();
        initApp();
        inAppNotification = new InAppNotification(this);
        initNotificationChannel();
        if (BuildConfig.DEBUG) {
            FirebaseMessaging.getInstance().subscribeToTopic("test");
        }
        Window window = this.getWindow();
        window.addFlags(WindowManager.LayoutParams.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS);
        window.clearFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS);
        window.setStatusBarColor(this.getResources().getColor(R.color.colorPrimaryDark, getTheme()));
        countAppUsageDays();
    }

    private void initNotificationChannel() {
        NotificationUtils.createNotificationChannel(this, NotificationUtils.CHANNEL_ID);
        NotificationUtils.createNotificationChannel(this, NotificationUtils.FLOATING_NOTIFICATION);
        NotificationUtils.createNotificationChannel(this, NotificationUtils.RINGING_CHANNEL_ID);
        NotificationUtils.createNotificationChannel(this, NotificationUtils.TRIP_CHANNEL_ID);
        NotificationUtils.createNotificationChannel(this, NotificationUtils.CANCELLED_PRODUCT);
        NotificationUtils.createNotificationChannel(this, NotificationUtils.DRIVER_HAS_REACHED);
    }

    public void updateConfigURL() {
        String key = MERCHANT_TYPE;
        String merchantId = key.equals("USER") ? in.juspay.mobility.BuildConfig.MERCHANT_ID_USER : in.juspay.mobility.BuildConfig.MERCHANT_ID_DRIVER;
        String baseUrl = key.equals("USER") ? in.juspay.mobility.BuildConfig.CONFIG_URL_USER : in.juspay.mobility.BuildConfig.CONFIG_URL_DRIVER;
        SharedPreferences sharedPreff = this.getSharedPreferences(
                activity.getString(R.string.preference_file_key), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = sharedPreff.edit();
        editor.putString("MERCHANT_ID", merchantId);
        editor.putString("BASE_URL", baseUrl);
        editor.apply();
    }

    public void triggerPopUPMain(String id, String type) {

        try {
            Log.i(LOG_TAG, "Triggering the process");
            hyperServices.process(new JSONObject().put("service", "in.yatri.consumer").put("requestId", UUID.randomUUID()).put("payload", new JSONObject().put("action", "showPopup").put("id", id).put("popType", type)));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void initApp() {

        hyperServices = new HyperServices(this, findViewById(in.juspay.mobility.app.R.id.cl_dui_container));
        final JSONObject json = new JSONObject();
        JSONObject payload = new JSONObject();

        try {

            json.put("requestId", "123");
            json.put("service", getService());
            json.put("betaAssets", false);
            payload.put("clientId","mobilitypaytmconsumer");
            payload.put("action", "initiate");
            payload.put("service", getService());
            payload.put(PaymentConstants.ENV, "master");

//            JSONObject signatureAuthData = new JSONObject();
//            signatureAuthData.put("signature", "Tz8ew9MYewcXBKIkjT7U+Tu3bPN06RZHBIKKbaJjMQ+e5uTaI4Hz0Ktu2KAXITR+7xBhBaLkMZ4Fb6HyaEOUjZES/qid/cVghyi1rJn3A0mI4VmMGt50IOep0b+5Ae2N1yCz58SwWvIRunv345amE0URHD6uca71rk2Rijva5XGjwgNrqOWXpzrHT0y0FRrvEr4u3du8QS2q0Wu4fZ2Ps9RSh04iPVNNuuMcUgkGSktxFP5vJLVYllYJDUzrOxi7nq3R11utNlSQMu18+ATSO5HyMTLxpndjhFlUJqn4QGxYovpp7amztJYjvoEnG9itPp2WdYamVeRAEcJnqRon2w==");
//            signatureAuthData.put("authData", "{\"mobileNumber\":\"9642429378\",\"mobileCountryCode\":\"+91\",\"merchantId\":\"NAMMA_YATRI\",\"timestamp\":\"2023-04-13T07:28:40+00:00\"}");
//            payload.put("signatureAuthData", signatureAuthData);

            json.put(PaymentConstants.PAYLOAD, payload);
        } catch (JSONException e) {
            e.printStackTrace();
        }
        hyperServices.initiate(json, new HyperPaymentsCallbackAdapter() {
            @Override
            public void onEvent(JSONObject jsonObject, JuspayResponseHandler juspayResponseHandler) {
                Log.d(LOG_TAG, "onEvent: " + jsonObject.toString());
                String event = jsonObject.optString("event");
                if (event.equals("initiate_result")) {
                    //Getting Notification Data from Bundle and attaching to process payload
                    if (getIntent().hasExtra("NOTIFICATION_DATA") || (getIntent().hasExtra("notification_type") && getIntent().hasExtra("entity_ids") && getIntent().hasExtra("entity_type"))) {
                        try {
                            System.out.println("It has entered if statment");
                            JSONObject payload1 = json.getJSONObject(PaymentConstants.PAYLOAD);
                            payload1.put("action", "process");
                            payload1.put("notificationData", getNotificationDataFromIntent());
                            json.put(PaymentConstants.PAYLOAD, payload1);
                        } catch (JSONException e) {
                            Log.e("NOTIFICATIONDATA", e.toString());
                        }
                    }
                    Log.e(LOG_TAG, "json_payload" + json);
                    hyperServices.process(json);
                } else if (event.equals("hide_loader") || event.equals("hide_splash")) {
                    String key = getResources().getString(R.string.service);
                    if (key.equals("nammayatri") && isSystemAnimEnabled) {
                        isHideSplashEventCalled = true;
                    } else {
                        hideSplash();
                    }
                } else if (event.equals("show_splash")) {
                    View v = findViewById(in.juspay.mobility.app.R.id.splash);
                    if (v != null) {
                        findViewById(in.juspay.mobility.app.R.id.splash).setVisibility(View.VISIBLE);
                    }
                } else if (event.equals("reboot")) {
                    Log.i(LOG_TAG, "event reboot");
                    hyperServices.terminate();
                    hyperServices = null;
                    initApp();
                } else if(jsonObject.optString("event").equals("in_app_notification")){
                    String title = jsonObject.optString("title");
                    String message = jsonObject.optString("message");
                    String channelId = jsonObject.optString("channelId");
                    String action1Text = jsonObject.optString("action1Text") ;
                    String action2Text = jsonObject.optString("action2Text");
                    String action1Image = jsonObject.optString("action1Image") ;
                    String action2Image = jsonObject.optString("action2Image");
                    String onTapAction = jsonObject.optString("onTapAction");
                    int durationInMilliSeconds = Integer.parseInt(jsonObject.optString("durationInMilliSeconds"));
                    showInAppNotification(title, message, onTapAction, action1Text,action2Text , action1Image,action2Image , channelId , durationInMilliSeconds, context);
                } else if (event.equals("in_app_notification")) {
                    showInAppNotifiation(jsonObject.optString("title"), jsonObject.optString("message"));
                } else if (event.equals("location_permission")) {
                    try {
                        JSONObject payload1 = json.getJSONObject(PaymentConstants.PAYLOAD);
                        payload1.put("action", "location_permission_result");
                        json.put(PaymentConstants.PAYLOAD, payload1);
                    } catch (Exception e) {
                        Log.e(LOG_TAG,"Exception in location_permission");
                    }
                    hyperServices.process(json);
                } else if (event.equals("process_result")) {
                    try {
                        JSONObject payload1 = json.getJSONObject(PaymentConstants.PAYLOAD);
                        if (payload1.getString("action").equals("terminate")) {
                            Intent startMain = new Intent(Intent.ACTION_MAIN);
                            startMain.addCategory(Intent.CATEGORY_HOME);
                            startMain.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                            context.startActivity(startMain);
                        }
                    } catch (Exception ignored) {
                    }
                }
            }
        });
    }

    public void showAlertForUpdate() {
        System.out.println("inside showAlertForUpdate");
        AlertDialog.Builder builder = new AlertDialog.Builder(MainActivity.this);
        builder.setCancelable(false);
        ConstraintLayout constraintLayout = (ConstraintLayout) getLayoutInflater().inflate(in.juspay.mobility.app.R.layout.dynamic_update_loader, null);
        CardView cardView = constraintLayout.findViewById(in.juspay.mobility.app.R.id.apiLoaderOverlayCard);
        cardView.setCardElevation(0);
        cardView.setRadius(0);

        ViewGroup.LayoutParams layoutParams = new ConstraintLayout.LayoutParams(ConstraintLayout.LayoutParams.MATCH_PARENT, ConstraintLayout.LayoutParams.WRAP_CONTENT);
        constraintLayout.setLayoutParams(layoutParams);
        builder.setView(constraintLayout);
        builder.setPositiveButton(in.juspay.mobility.app.R.string.okay_got_it, (dialog, which) -> {
            dialog.cancel();
            hyperServices.terminate();
            hyperServices = null;
            initApp();
        });
        runOnUiThread(() -> {
            AlertDialog alertDialog = builder.create();
            alertDialog.show();
        });
    }

    private JSONObject getNotificationDataFromIntent() throws JSONException {
        Bundle bundle = getIntent().getExtras();
        JSONObject data;
        //Handling local and foreground notifications
        if (getIntent().hasExtra("NOTIFICATION_DATA")) {
            data = new JSONObject(bundle.getString("NOTIFICATION_DATA"));
        }
        //Handling background notifications
        else if (getIntent().hasExtra("notification_type") && getIntent().hasExtra("entity_ids") && getIntent().hasExtra("entity_type")) {
            data = new JSONObject();
            data.put("notification_type", bundle.getString("notification_type"));
            data.put("entity_ids", bundle.getString("entity_ids"));
            data.put("entity_type", bundle.getString("entity_type"));
        } else {
            data = new JSONObject();
        }
        return data;
    }

    @Override
    protected void onNewIntent(Intent intent) {
        if (intent != null && intent.hasExtra("NOTIFICATION_DATA")) {
            String data = intent.getExtras().getString("NOTIFICATION_DATA");
            try {
                JSONObject jsonData = new JSONObject(data);
                if(jsonData.has("notification_type") && jsonData.getString("notification_type").equals("CHAT_MESSAGE")){
                    hyperServices.process(new JSONObject().put("service", "in.juspay." + getResources().getString(R.string.service)).put("requestId", UUID.randomUUID()).put("payload", new JSONObject().put("action", "OpenChatScreen").put("notification_type", "CHAT_MESSAGE")));
                }
                if (jsonData.has("notification_type") && jsonData.has("entity_ids")) {
                    String id = jsonData.getString("entity_ids");
                    String type = jsonData.getString("notification_type");
                    if (type.equals("NEW_MESSAGE")) {
                        hyperServices.process(new JSONObject().put("service", getService()).put("requestId", UUID.randomUUID()).put("payload", new JSONObject().put("action", "callDriverAlert").put("id", id).put("popType", type)));
                    }
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        super.onNewIntent(intent);
    }

    @Override
    protected void onResume() {
        super.onResume();
        if (sharedPref != null) {
            sharedPref.edit().putString(getResources().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onResume").apply();
            sharedPref.edit().putString("MAPS_OPENED", "null").apply();
        }
        appUpdateManager.getAppUpdateInfo().addOnSuccessListener(appUpdateInfo -> {
            if (appUpdateInfo.updateAvailability() == UpdateAvailability.DEVELOPER_TRIGGERED_UPDATE_IN_PROGRESS) {
                // If an in-app update is already running, resume the update.
                try {
                    appUpdateManager.startUpdateFlowForResult(
                            appUpdateInfo,
                            AppUpdateType.IMMEDIATE,
                            this,
                            getResources().getInteger(REQUEST_CODE_UPDATE_APP)
                    );
                } catch (IntentSender.SendIntentException e) {
                    e.printStackTrace();
                }
            }
        });
        if (getResources().getString(R.string.service).equals("DRIVER")) {
            if (NotificationUtils.overlayFeatureNotAvailable(this)) {
                checkRideRequest();
            }
            if (widgetService != null) {
                stopService(widgetService);
            }
        }
    }

    @Override
    protected void onPause() {
        super.onPause();
        if (sharedPref != null)
            sharedPref.edit().putString(getResources().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onPause").apply();
        if (getResources().getString(R.string.service).equals("nammayatripartner") && widgetService != null && Settings.canDrawOverlays(this) && !sharedPref.getString(getResources().getString(in.juspay.mobility.app.R.string.REGISTERATION_TOKEN), "null").equals("null")) {
            widgetService.putExtra("payload", "{}");
            widgetService.putExtra("data", "{}");
            startService(widgetService);
        }
    }

    @Override
    protected void onDestroy() {
        if (sharedPref != null) {
            sharedPref.edit().putString(getResources().getString(in.juspay.mobility.app.R.string.ACTIVITY_STATUS), "onDestroy").apply();
            String role = sharedPref.getString("ROLE_KEY", "null");
            String location_status = sharedPref.getString("LOCATION_STATUS", "PAUSE");
            System.out.println("Outside onDestroy Driver" + role);
            if (role.equals("DRIVER") && location_status.equals("START") && (ActivityCompat.checkSelfPermission(getApplicationContext(), Manifest.permission.ACCESS_FINE_LOCATION) == PackageManager.PERMISSION_GRANTED && ActivityCompat.checkSelfPermission(getApplicationContext(), Manifest.permission.ACCESS_COARSE_LOCATION) == PackageManager.PERMISSION_GRANTED)) {
                Intent broadcastIntent = new Intent();
                broadcastIntent.setAction("restartservice");
                broadcastIntent.setClass(this, BootUpReceiver.class);
                this.sendBroadcast(broadcastIntent);
            }
        }
        if (hyperServices != null) {
            hyperServices.terminate();
        }
        unregisterReceiver(gpsReceiver);
        unregisterReceiver(networkBroadcastReceiver);
        NetworkBroadcastReceiver.deRegisterProcessCallback(processCallBack);
        MyFirebaseMessagingService.deRegisterBundleUpdateCallback(bundleUpdateCallBack);
        super.onDestroy();
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        switch (requestCode) {
            case IMAGE_CAPTURE_REQ_CODE:
                CommonJsInterface.isUploadPopupOpen = false;
                if (resultCode == RESULT_OK) {
                    captureImage (data);
                }
                break;
            case REQUEST_CODE_UPDATE_APP:
                if (resultCode != RESULT_OK) {
                    Log.i(TAG,"Update flow failed! Result code: " + resultCode);
                    if(updateType == AppUpdateType.IMMEDIATE){
                        finishAndRemoveTask();
                    }
                }
                break;
            case CropImage.CROP_IMAGE_ACTIVITY_REQUEST_CODE:
                if (resultCode == RESULT_OK) {
                    new Thread(new Runnable() {
                        @Override
                        public void run() {
                            encodeImageToBase64 (data);
                        }
                    }).start();
                } else if (resultCode == CropImage.CROP_IMAGE_ACTIVITY_RESULT_ERROR_CODE) {
                    CropImage.ActivityResult result = CropImage.getActivityResult(data);
                    Log.e(TAG,result.getError().toString());
                }
                break;
            default:return;
        }
    }

  @Override
  public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
      super.onRequestPermissionsResult(requestCode, permissions, grantResults);
      switch (requestCode) {
          case IMAGE_PERMISSION_REQ_CODE :
              if ((ActivityCompat.checkSelfPermission(this, WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED) && (ActivityCompat.checkSelfPermission(this, CAMERA) == PackageManager.PERMISSION_GRANTED) && (ActivityCompat.checkSelfPermission(this, READ_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED)){
                  Intent takePicture = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
                  String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss", new Locale("en","US")).format(new Date());
                  sharedPref.edit().putString(getResources().getString(R.string.TIME_STAMP_FILE_UPLOAD), timeStamp).apply();
                  Uri photoFile = FileProvider.getUriForFile(this.getApplicationContext(), getApplicationInfo().packageName + ".fileProvider", new File(this.getApplicationContext().getFilesDir(), "IMG_" + timeStamp+".jpg"));
                  takePicture.putExtra(MediaStore.EXTRA_OUTPUT, photoFile);
                  Intent chooseFromFile = new Intent(Intent.ACTION_GET_CONTENT);
                  chooseFromFile.setType("image/*");
                  Intent chooser = Intent.createChooser(takePicture, getString(R.string.upload_image));
                  chooser.putExtra(Intent.EXTRA_INITIAL_INTENTS, new Intent[] { chooseFromFile });
                  this.startActivityForResult(chooser,IMAGE_CAPTURE_REQ_CODE);
              } else {
                  Toast.makeText(this, getString(R.string.please_allow_permission_to_capture_the_image), Toast.LENGTH_SHORT).show();
              }
              break;
          case CommonJsInterface.REQUEST_CALL :
              if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                  Intent intent = new Intent(Intent.ACTION_CALL,Uri.parse("tel:"+CommonJsInterface.phoneNumber));
                  this.startActivity(intent);
              }else{
                  enablePermissionFromSettings(Manifest.permission.CALL_PHONE, "Phone");
              }
              break;
          case CommonJsInterface.LOCATION_PERMISSION_REQ_CODE:
              if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                  System.out.println("Location Permission Granted");
              }else{
                  enablePermissionFromSettings(Manifest.permission.ACCESS_FINE_LOCATION, "Location");
              }
              break;
          case CommonJsInterface.STORAGE_PERMISSION:
              if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                  try {
                      CommonJsInterface.downloadPDF(CommonJsInterface.invoice , (Activity) this,this);
                  } catch (JSONException e) {
                      e.printStackTrace();
                  }
              }else {
                  Toast.makeText(this, "Permission Denied", Toast.LENGTH_SHORT).show();
              }
              break;
          case AudioRecorder.REQUEST_RECORD_AUDIO_PERMISSION:
              if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                  AudioRecorder.recordPermissionAccepted();
              } else {
                  Toast.makeText(this, "Permission Denied", Toast.LENGTH_SHORT).show();
              }
              break;
          case CommonJsInterface.REQUEST_CONTACTS:
              boolean flag = ContextCompat.checkSelfPermission(MainActivity.getInstance(), Manifest.permission.READ_CONTACTS) == PackageManager.PERMISSION_GRANTED;
              String contacts = null;
              try {
                  if (flag){
                      contacts = getPhoneContacts();
                  } else {
                      JSONArray flagArray = new JSONArray();
                      contacts = flagArray.toString();
                  }
                  if (juspayServicesGlobal.getDynamicUI() != null) {
                      CommonJsInterface.contactsStoreCall(juspayServicesGlobal.getDuiCallback(), contacts);
                  }
              } catch (JSONException e) {
                  e.printStackTrace();
              }
              break;
          default: return;
      }
  }

    public String getPhoneContacts() throws JSONException {
        ContentResolver contentResolver = getContentResolver();
        Uri uri = ContactsContract.CommonDataKinds.Phone.CONTENT_URI;
        Cursor cursor = contentResolver.query(uri,null,null,null,null);

        JSONArray contacts = new JSONArray();

        if(cursor.getCount()>0){
            while(cursor.moveToNext()){
                String contactNameStr = cursor.getString(cursor.getColumnIndexOrThrow(ContactsContract.CommonDataKinds.Phone.DISPLAY_NAME));
                String contactStr = cursor.getString(cursor.getColumnIndexOrThrow(ContactsContract.CommonDataKinds.Phone.NUMBER));
                String contactNumber = contactStr.replaceAll("[^0-9]", "");
                String contactName = contactNameStr.replaceAll("'","");
                JSONObject tempPoints = new JSONObject();
                tempPoints.put("name",contactName);
                tempPoints.put("number",contactNumber);
                contacts.put(tempPoints);
            }
        }

        JSONObject flagObject = new JSONObject();
        flagObject.put("name","beckn_contacts_flag");
        flagObject.put("number","true");
        contacts.put(flagObject);
        System.out.print("Contacts " + contacts);
        return contacts.toString();
    }

    public void firstTimeAskingPermission(Context context, String permission){
        SharedPreferences sharedPreference = context.getSharedPreferences(activity.getString(R.string.preference_file_key), MODE_PRIVATE);
        sharedPreference.edit().putString(permission, "false").apply();
    }

    public String isFirstTimeAskingPermission(Context context, String permission){
        return context.getSharedPreferences(activity.getString(R.string.preference_file_key), MODE_PRIVATE).getString(permission, "true");
    }

    public void enablePermissionFromSettings(@NonNull String permission, String permissionName){
//        if(!isFirstTimeAskingPermission(this, Manifest.permission.ACCESS_FINE_LOCATION))

        if (ActivityCompat.shouldShowRequestPermissionRationale(this, permission)){
            firstTimeAskingPermission(this, permission);
        }else{
            if(isFirstTimeAskingPermission(this, permission).equals("false")){
                try {
                    LayoutInflater inflater = (this).getLayoutInflater();
                    View permissionStepsView = ((LayoutInflater) this.getSystemService(Context.LAYOUT_INFLATER_SERVICE)).inflate(R.layout.permission_steps_layout, null);
                    TextView stepText = permissionStepsView.findViewById(R.id.step_text);

                    AlertDialog.Builder builder = new AlertDialog.Builder(this);
                    stepText.setText("3. Tap on "+permissionName);
                    builder.setTitle("Permission Required")
                            .setCancelable(true)
                            .setView(permissionStepsView)
                            .setPositiveButton("Go to settings", new DialogInterface.OnClickListener() {
                                public void onClick(DialogInterface dialog, int which) {
                                    Intent settingsIntent = new Intent(Settings.ACTION_APPLICATION_DETAILS_SETTINGS);
                                    Uri uri = Uri.fromParts("package", getPackageName(), null);
                                    settingsIntent.setData(uri);
                                    startActivity(settingsIntent);
                                }
                            });
                    AlertDialog alert = builder.create();
                    alert.show();
                } catch (Exception e) {
                    Log.d("error", e.toString());
                }
            }
        }
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        hyperServices.onRequestPermissionsResult(requestCode, permissions, grantResults);
    }

    public void hideSplash() {
        View v = findViewById(in.juspay.mobility.app.R.id.cl_dui_container);
        if (v != null) {
            findViewById(in.juspay.mobility.app.R.id.cl_dui_container).setVisibility(View.VISIBLE);
        }
        View splashView = findViewById(in.juspay.mobility.app.R.id.splash);
        if (splashView != null) {
            splashView.setVisibility(View.GONE);
        }
    }

    private void countAppUsageDays() {
        Date currentDate = new Date();
        SharedPreferences sharedPref = this.getSharedPreferences(this.getString(in.juspay.mobility.app.R.string.preference_file_key), Context.MODE_PRIVATE);
        long millis = sharedPref.getLong("PREVIOUS_USED_DATE", 0L);
        if (millis == 0L) {
            sharedPref.edit().putLong("PREVIOUS_USED_DATE", currentDate.getTime()).apply();
        }
        Date previousDate = new Date(sharedPref.getLong("PREVIOUS_USED_DATE", 0L));
        if (TimeUnit.MILLISECONDS.toDays(currentDate.getTime() - previousDate.getTime()) > 0) {
            // update days Count
            sharedPref.edit().putInt("DAYS_COUNT", sharedPref.getInt("DAYS_COUNT", 0) + 1).apply();
            sharedPref.edit().putString("USED_DAYS_COUNT", String.valueOf(sharedPref.getInt("DAYS_COUNT", 0))).apply();
            // update previousDate to currentDate
            sharedPref.edit().putLong("PREVIOUS_USED_DATE", currentDate.getTime()).apply();
        }
    }

    private void captureImage (@Nullable Intent data) {
        try {
            Uri imageUri;
            if (data == null || data.getData() == null) { //Camera
                File image = new File(this.getApplicationContext().getFilesDir(), "IMG_" + sharedPref.getString(getResources().getString(R.string.TIME_STAMP_FILE_UPLOAD), "null") + ".jpg");
                imageUri = FileProvider.getUriForFile(this.getApplicationContext(), getApplicationInfo().packageName + ".fileProvider", image);
            }
            else { // storage
                imageUri = data.getData();
            }
            startCropImageActivity(imageUri);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void startCropImageActivity(Uri imageUri){
        CropImage.activity(imageUri)
                .setAllowFlipping(false)
                .start(this);
    }

    private void encodeImageToBase64(@Nullable Intent data) {
        try {
            CropImage.ActivityResult result = CropImage.getActivityResult(data);
            Uri fileUri = result.getUri();
            InputStream imageStream = getContentResolver().openInputStream(fileUri);
            Bitmap selectedImage = BitmapFactory.decodeStream(imageStream);
            ByteArrayOutputStream baos = new ByteArrayOutputStream();

            byte[] b;
            String encImage;

            selectedImage.compress(Bitmap.CompressFormat.JPEG, 100, baos);
            b = baos.toByteArray();
            encImage = Base64.encodeToString(b, Base64.NO_WRAP);

            Log.d(TAG, "camera image size : " + String.valueOf((Math.ceil(encImage.length() / 4) * 3) / 1000));

            if ((Math.ceil(encImage.length() / 4) * 3) / 1000 > 400) {
                Integer reduceQuality = 10;
                selectedImage.compress(Bitmap.CompressFormat.JPEG, 100 - reduceQuality, baos);
                b = baos.toByteArray();
                encImage = Base64.encodeToString(b, Base64.NO_WRAP);
                while ((Math.ceil(encImage.length() / 4) * 3) / 1000 > 400) {
                    if (reduceQuality >= 90) {
                        break;
                    }
                    reduceQuality += 10;
                    baos.reset();
                    selectedImage.compress(Bitmap.CompressFormat.JPEG, 100 - reduceQuality, baos);
                    b = baos.toByteArray();
                    encImage = Base64.encodeToString(b, Base64.NO_WRAP);
                }
            }

            Log.d(TAG, "encoded image size camera : " + String.valueOf((Math.ceil(encImage.length() / 4) * 3) / 1000));
            if (juspayServicesGlobal.getDynamicUI() != null) {
                String timeStamp = new SimpleDateFormat("yyyyMMdd_HHmmss", new Locale("en","US")).format(new Date());
                CommonJsInterface.callingStoreCallImageUpload(juspayServicesGlobal.getDuiCallback(), encImage, "IMG_" + timeStamp +".jpg", result.getUri().getPath());
            }
        }
        catch (Exception e){
            e.printStackTrace();
            Bundle params = new Bundle();
            mFirebaseAnalytics.logEvent("exception_crop_image", params);
        }
    }

    public static void showInAppNotification(String title, String message, String onTapAction, String action1Text, String action2Text, String action1Image, String action2Image, String channelId, int durationInMilliSeconds, Context context) {
        try {
            Handler handler = new Handler(context.getMainLooper());
            handler.postDelayed(() -> {
                try {
                    inAppNotification.generateNotification(title, message, onTapAction, action1Text, action2Text, action1Image, action2Image, channelId, durationInMilliSeconds);
                } catch (JSONException e) {
                    Log.e(TAG, "Error in In App Notification Handler " + e);
                }
            }, 0);
        } catch (Exception e) {
            Log.e(TAG, "Error in In App Notification " + e);
        }
    }

    private void checkRideRequest() {
        try {
            boolean rideReqExpired = NotificationUtils.lastRideReq.getBoolean("rideReqExpired", true);
            if (rideReqExpired) return;
            Intent rideReqActivity = new Intent(this, RideRequestActivity.class);
            rideReqActivity.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_REORDER_TO_FRONT);
            rideReqActivity.putExtras(NotificationUtils.lastRideReq);
            startActivity(rideReqActivity);
        } catch (Exception e) {
            Log.e(LOG_TAG, "Exception in checkRideRequest");
        }
    }

    static class NotificationListener extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            Log.e("In Main activity", context.toString());
        }
    }
    public String getService () {
        return "in.yatri.consumer";
//        StringBuilder key = new StringBuilder();
//        if (in.juspay.mobility.BuildConfig.MERCHANT.equals("KL")) {
//            key.append("net.openkochi.");
//        } else {
//            key.append("in.juspay.");
//        }
//        key.append(getResources().getString(R.string.service));
//        return key.toString();
    }
}
