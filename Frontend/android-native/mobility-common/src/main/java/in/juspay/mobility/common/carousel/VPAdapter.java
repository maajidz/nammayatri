package in.juspay.mobility.common.carousel;

import android.content.Context;
import android.content.Intent;
import android.content.res.Resources;
import android.graphics.Color;
import android.graphics.drawable.GradientDrawable;
import android.text.Html;
import android.util.Log;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.JavascriptInterface;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.YouTubePlayer;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.AbstractYouTubePlayerListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.YouTubePlayerFullScreenListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.listeners.YouTubePlayerListener;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.options.IFramePlayerOptions;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.player.views.YouTubePlayerView;
import com.pierfrancescosoffritti.androidyoutubeplayer.core.ui.DefaultPlayerUiController;

import org.json.JSONObject;

import java.util.ArrayList;

import in.juspay.hyper.core.ExecutorManager;
import in.juspay.mobility.app.R;
import in.juspay.mobility.common.YoutubeVideoView;

public class VPAdapter extends RecyclerView.Adapter<VPAdapter.ViewHolder> {
    ArrayList<ViewPagerItem> viewPagerItemArrayList;
    public static float videoDuration = 0;

    public static YouTubePlayerView youTubePlayerView ;
    public static YouTubePlayer youtubePlayer;
    public static Context context ;
    public VPAdapter(ArrayList<ViewPagerItem> viewPagerItemArrayList, Context context) {
        this.viewPagerItemArrayList = viewPagerItemArrayList;
        this.context = context;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.viewpager_item,parent,false);
        return new ViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ViewHolder holder, int position) {
        try{
            ViewPagerItem viewPagerItem = viewPagerItemArrayList.get(position);
            JSONObject imageConfig = viewPagerItem.imageConfig;
            JSONObject descriptionConfig = viewPagerItem.descriptionConfig ;
            JSONObject titleConfig = viewPagerItem.titleConfig;
            JSONObject margin = viewPagerItem.descriptionConfig.getJSONObject("margin");
            JSONObject titleMargin = viewPagerItem.titleConfig.getJSONObject("margin");
            String titleGravity = titleConfig.getString("gravity");
            int carouselGravity = viewPagerItem.gravity;
            String descriptionGravity = descriptionConfig.getString("gravity");

            float density = (Resources.getSystem().getDisplayMetrics().density);
            // imageView Config ------------------------------------------
            if ((viewPagerItem.contentType).equals("IMAGE")){
                holder.imageView.setImageResource(viewPagerItem.imageID);
                holder.imageView.getLayoutParams().height = (int) (imageConfig.getInt("height") * density);
                GradientDrawable gradientDrawable = new GradientDrawable();
                gradientDrawable.setShape(GradientDrawable.RECTANGLE);
                holder.imageView.setVisibility(View.VISIBLE);
                gradientDrawable.setCornerRadii(new float[] {20, 20, 20, 20, 0,0,0,0});
                gradientDrawable.setColor(Color.parseColor(viewPagerItem.imageConfig.getString("bgColor")));
                holder.imageView.setBackground(gradientDrawable);
                holder.video.setVisibility(View.GONE);
            }
            else {
                embedYoutubeVideo(context, (viewPagerItem.videoData).toString(), String.valueOf(R.id.videoViewLinearLayout), "PLAY", holder.video);
                holder.video.setVisibility(View.VISIBLE);
                holder.imageView.setVisibility(View.GONE);
            }

            // Heading text Config ------------------------------------------
            holder.tvHeading.setTextSize(titleConfig.getInt("textSize"));
            holder.tvHeading.setTextColor(Color.parseColor(titleConfig.getString("textColor")));
            holder.tvHeading.setText(titleConfig.getString("text"));
            ViewGroup.MarginLayoutParams titleLayoutParams = (ViewGroup.MarginLayoutParams) holder.tvHeading.getLayoutParams();
            titleLayoutParams.setMargins(titleMargin.getInt("left"), titleMargin.getInt("top"), titleMargin.getInt("right"), titleMargin.getInt("bottom"));
            holder.tvHeading.setLayoutParams(titleLayoutParams);
            holder.tvHeading.setGravity(getGravity(titleGravity));

            // Description text Config ---------------------------------------
            holder.tvDesc.setText(Html.fromHtml(descriptionConfig.getString("text")));
            if(descriptionConfig.getString("text").equals(""))
            {
                holder.tvDesc.setVisibility(View.GONE);
            }
            holder.tvDesc.setTextSize(descriptionConfig.getInt("textSize"));
            holder.tvDesc.setTextColor(Color.parseColor(descriptionConfig.getString("textColor")));
            ViewGroup.MarginLayoutParams descLayoutParams = (ViewGroup.MarginLayoutParams) holder.tvDesc.getLayoutParams();
            descLayoutParams.setMargins(margin.getInt("left") * 2, margin.getInt("top"), margin.getInt("right"), margin.getInt("bottom"));
            holder.tvDesc.setLayoutParams(descLayoutParams);
            holder.tvDesc.setGravity(getGravity(descriptionGravity));
            holder.parentLinearLayout.setGravity(carouselGravity);
          }
            catch (Exception e){

        }
    }
    private int getGravity(String gravity){
        switch (gravity){
            case "LEFT": return Gravity.LEFT;
            case "RIGHT" : return Gravity.RIGHT;
            case "TOP" :  return Gravity.TOP;
            case "BOTTOM" : return Gravity.BOTTOM;
            default: return Gravity.CENTER;}
    }
    @Override
    public int getItemCount() {
        return viewPagerItemArrayList.size();
    }

    public static class ViewHolder extends RecyclerView.ViewHolder{
        ImageView imageView;
        LinearLayout video, parentLinearLayout;
        TextView tvHeading, tvDesc;
        public ViewHolder(@NonNull View itemView) {
            super(itemView);
            imageView = itemView.findViewById(R.id.carouselImageView);
            tvHeading = itemView.findViewById(R.id.tvHeading);
            tvDesc = itemView.findViewById(R.id.tvDesc);
            video = itemView.findViewById(R.id.videoViewLinearLayout);
            parentLinearLayout = itemView.findViewById(R.id.parentLinearLayout);
        }
    }

    public void embedYoutubeVideo(Context context, String rawJson, final String playerId, String videoStatus, LinearLayout video) {
            videoDuration = 0;
            ExecutorManager.runOnMainThread(() -> {
                try {
                    if (videoStatus.equals("PAUSE")) {
                        pauseYoutubeVideo();
                    } else {
                        JSONObject json = new JSONObject(rawJson);
                        boolean showMenuButton = json.getBoolean("showMenuButton");
                        boolean showDuration = json.getBoolean("showDuration");
                        boolean setVideoTitle = json.getBoolean("setVideoTitle");
                        boolean showSeekBar = json.getBoolean("showSeekBar");
                        String videoTitle = json.getString("videoTitle");
                        String videoId = json.getString("videoId");
                        String videoType = "VIDEO";
                        int videoHeight = json.getInt("videoHeight");
                        if (json.has("videoType")) {
                            videoType = json.getString("videoType");
                        }
                        youTubePlayerView = new YouTubePlayerView(context);
                        LinearLayout layout = video ;
                        layout.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.MATCH_PARENT , videoHeight));
                        layout.addView(youTubePlayerView);
                        youTubePlayerView.setEnableAutomaticInitialization(false);
                        YouTubePlayerListener youTubePlayerListener = new AbstractYouTubePlayerListener() {
                            @Override
                            public void onReady(@NonNull YouTubePlayer youTubePlayer) {
                                try {
                                    youtubePlayer = youTubePlayer;
                                    DefaultPlayerUiController playerUiController = new DefaultPlayerUiController(youTubePlayerView, youTubePlayer);
                                    playerUiController.showMenuButton(showMenuButton);
                                    playerUiController.showDuration(showDuration);
                                    playerUiController.showSeekBar(showSeekBar);
                                    playerUiController.showFullscreenButton(true);
                                    if (setVideoTitle) {
                                        playerUiController.setVideoTitle(videoTitle);
                                    }
                                    playerUiController.showYouTubeButton(false);
                                    youTubePlayerView.setCustomPlayerUi(playerUiController.getRootView());
                                    youTubePlayer.seekTo(videoDuration);
                                    youTubePlayer.loadVideo(videoId, 0);
                                    youTubePlayer.play();

                                } catch (Exception e) {
                                    Log.e("error inside embedYoutubeVideo onReady", String.valueOf(e));
                                }
                            }

                            @Override
                            public void onCurrentSecond(@NonNull YouTubePlayer youTubePlayer, float second) {
                                videoDuration = second;
                            }
                        };
                        if (videoHeight != 0 )
                        {   ViewGroup.LayoutParams layoutParams = youTubePlayerView.getLayoutParams();
                            layoutParams.height = videoHeight;
                            youTubePlayerView.setLayoutParams(layoutParams);
                            youTubePlayerView.setMinimumHeight(700);
                        }

                        String finalVideoType = videoType;
                        youTubePlayerView.addFullScreenListener(new YouTubePlayerFullScreenListener() {
                            @Override
                            public void onYouTubePlayerExitFullScreen() {
                                if (videoHeight != 0 )
                                    {   ViewGroup.LayoutParams layoutParams = youTubePlayerView.getLayoutParams();
                                        layoutParams.height = videoHeight;
                                        youTubePlayerView.setLayoutParams(layoutParams);
                                        youTubePlayerView.setMinimumHeight(700);
                                    }
                            }

                            @Override
                            public void onYouTubePlayerEnterFullScreen() {
                                Intent newIntent = new Intent(context, YoutubeVideoView.class);
                                newIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                                newIntent.putExtra("videoId", videoId);
                                newIntent.putExtra("videoDuration", videoDuration);
                                newIntent.putExtra("videoType", finalVideoType);
                                context.startActivity(newIntent);
                            }
                        });

                        IFramePlayerOptions options = new IFramePlayerOptions.Builder().controls(0).rel(0).build();
                        youTubePlayerView.initialize(youTubePlayerListener, options);
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            });
    }

    @JavascriptInterface
    public static void pauseYoutubeVideo() {
        if (youTubePlayerView != null) {
            youtubePlayer.pause();
        }
    }


}