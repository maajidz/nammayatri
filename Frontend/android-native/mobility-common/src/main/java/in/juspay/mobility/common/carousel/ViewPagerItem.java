package in.juspay.mobility.common.carousel;

import org.json.JSONObject;

public class ViewPagerItem {
    int imageID, gravity;
    JSONObject imageConfig, descriptionConfig, titleConfig, videoData;
    String contentType;

    public ViewPagerItem(int imageID, JSONObject imageConfig, JSONObject descriptionConfig, JSONObject titleConfig, String contentType, JSONObject videoData , int gravity) {
        this.imageID = imageID;
        this.imageConfig =  imageConfig;
        this.descriptionConfig = descriptionConfig;
        this.titleConfig = titleConfig;
        this.videoData = videoData;
        this.contentType = contentType;
        this.gravity = gravity;
    }
}