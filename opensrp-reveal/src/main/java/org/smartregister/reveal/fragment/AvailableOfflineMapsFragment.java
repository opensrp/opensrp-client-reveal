package org.smartregister.reveal.fragment;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v4.content.LocalBroadcastManager;
import android.support.v7.widget.RecyclerView;
import android.text.TextUtils;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.Toast;

import com.mapbox.mapboxsdk.geometry.LatLng;

import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.adapter.OfflineMapAdapter;
import org.smartregister.reveal.contract.AvailableOfflineMapsContract;
import org.smartregister.reveal.model.OfflineMapModel;

import java.util.List;

import io.ona.kujaku.helpers.OfflineServiceHelper;
import io.ona.kujaku.services.MapboxOfflineDownloaderService;
import io.ona.kujaku.utils.Constants;

public class AvailableOfflineMapsFragment extends Fragment implements AvailableOfflineMapsContract.View {

    private RecyclerView offlineMapRecyclerView;
    private OfflineMapAdapter adapter;
    private Button downloadMapButton;

    private AvailableOfflineMapsContract.Presenter presenter;

    private List<OfflineMapModel> offlineMapModelList;

    private String currentMapDownload;

    private MapDownloadReceiver mapDownloadReceiver = new MapDownloadReceiver();

    public static AvailableOfflineMapsFragment newInstance(Bundle bundle) {

        AvailableOfflineMapsFragment fragment = new AvailableOfflineMapsFragment();
        if (bundle != null) {
            fragment.setArguments(bundle);
        }
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
    }

    @Override
    public void onResume() {
        super.onResume();
        IntentFilter intentFilter = new IntentFilter(Constants.INTENT_ACTION_MAP_DOWNLOAD_SERVICE_STATUS_UPDATES);
        LocalBroadcastManager.getInstance(getActivity().getApplicationContext()).registerReceiver(mapDownloadReceiver, intentFilter);
    }

    @Override
    public void onPause() {
        super.onPause();
        LocalBroadcastManager.getInstance(getActivity().getApplicationContext()).unregisterReceiver(mapDownloadReceiver);
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_offline_map, container, false);
        setUpViews(view);
        initializeAdapter();
        return view;
    }

    private void setUpViews(View view) {
        offlineMapRecyclerView = view.findViewById(R.id.offline_map_recyclerView);

        downloadMapButton = view.findViewById(R.id.download_map);

        downloadMapButton.setOnClickListener(v -> downloadMap());

    }

    private void initializeAdapter() {
        adapter = new OfflineMapAdapter(this.getContext(), onClickListener);
        offlineMapRecyclerView.setAdapter(adapter);
        if (offlineMapModelList != null) {
            setOfflineMapModelList(offlineMapModelList);
        }

    }

    private View.OnClickListener onClickListener = new View.OnClickListener(){
        @Override
        public void onClick(View v) {
            OfflineMapModel model = (OfflineMapModel) offlineMapRecyclerView.getTag(R.id.offline_map_label);
            presenter.onDownloadAreaSelected();
        }
    };

    @Override
    public void setOfflineMapModelList(List<OfflineMapModel> offlineMapModelList) {
        if (adapter == null) {
            this.offlineMapModelList = offlineMapModelList;
        } else {
            adapter.setOfflineMapModels(offlineMapModelList);
            this.offlineMapModelList = null;
        }
    }

    @Override
    public void displayToast(String message) {
        Toast.makeText(getContext(), message, Toast.LENGTH_LONG).show();
    }

    @Override
    public void displayError(int title, String message) {
        Toast.makeText(getContext(), message, Toast.LENGTH_LONG).show();
    }

    private void downloadMap() {
        double topLeftLat = 37.7897;
        double topLeftLng = -119.5073;
        double bottomRightLat = 37.6744;
        double bottomRightLng = -119.6815;
        double topRightLat = 37.7897;
        double topRightLng = -119.6815;
        double bottomLeftLat = 37.6744;
        double bottomLeftLng = -119.5073;

        String mapName = "Op Area 1";

        currentMapDownload = mapName;

        String mapboxStyle = "mapbox://styles/ona/cj9jueph7034i2rphe0gp3o6m";
        LatLng topLeftBound = new LatLng(topLeftLat, topLeftLng);
        LatLng topRightBound = new LatLng(topRightLat, topRightLng);
        LatLng bottomRightBound = new LatLng(bottomRightLat, bottomRightLng);
        LatLng bottomLeftBound = new LatLng(bottomLeftLat, bottomLeftLng);

        double maxZoom = 20.0;
        double minZoom = 0.0;

        OfflineServiceHelper.ZoomRange zoomRange = new OfflineServiceHelper.ZoomRange(minZoom, maxZoom);

        OfflineServiceHelper.requestOfflineMapDownload(getActivity()
                , mapName
                , mapboxStyle
                , BuildConfig.MAPBOX_SDK_ACCESS_TOKEN
                , topLeftBound
                , topRightBound
                , bottomRightBound
                , bottomLeftBound
                , zoomRange
        );

    }

    private void stopMapDownLoad(@NonNull String mapName) {
        OfflineServiceHelper.stopMapDownload(getActivity(), mapName , BuildConfig.MAPBOX_SDK_ACCESS_TOKEN);
    }

    private class MapDownloadReceiver extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            Bundle bundle = intent.getExtras();
            if (bundle != null) {
                Log.i("KUJAKU SAMPLE APP TAG", intent.getExtras().toString());
                if (bundle.containsKey(MapboxOfflineDownloaderService.KEY_RESULT_STATUS)
                        && bundle.containsKey(MapboxOfflineDownloaderService.KEY_RESULT_MESSAGE)
                        && bundle.containsKey(MapboxOfflineDownloaderService.KEY_RESULTS_PARENT_ACTION)
                        && bundle.containsKey(Constants.PARCELABLE_KEY_MAP_UNIQUE_NAME)) {

                    String mapUniqueName = bundle.getString(Constants.PARCELABLE_KEY_MAP_UNIQUE_NAME);
                    String resultStatus = bundle.getString(MapboxOfflineDownloaderService.KEY_RESULT_STATUS);
                    MapboxOfflineDownloaderService.SERVICE_ACTION serviceAction = (MapboxOfflineDownloaderService.SERVICE_ACTION) bundle.get(MapboxOfflineDownloaderService.KEY_RESULTS_PARENT_ACTION);

                    String message = bundle.getString(MapboxOfflineDownloaderService.KEY_RESULT_MESSAGE);

                    if (MapboxOfflineDownloaderService.SERVICE_ACTION_RESULT.FAILED.name().equals(resultStatus)) {
                        if (!TextUtils.isEmpty(message)) {
                            if (!message.contains("MapBox Tile Count limit exceeded")) {
                                //showInfoNotification("Error occurred " + mapUniqueName + ":" + serviceAction.name(), message);

                                displayError(R.id.download_map,  message);
                            }
                        }

                        if (serviceAction == MapboxOfflineDownloaderService.SERVICE_ACTION.DELETE_MAP && !TextUtils.isEmpty(message)) {
                           displayError(R.id.download_map, message);
                        }
                                    /*
                                    (FACT) This is an error update from the service. If this is not
                                    a DELETE_MAP action and the update is about the map that we expect
                                    to be currently downloading, held by currentMapDownload variable, then we
                                    need to disable the STOP MAP DOWNLOAD since the download has already been
                                    stopped after the error. If we left this as true, then we would be misleading
                                    the user that they can stop a non-existent download.
                                     */
                        else if (!TextUtils.isEmpty(mapUniqueName) && mapUniqueName.equals(currentMapDownload)) {
                           // setCanStopMapDownload(false);
                        }
                    } else {
                        // We should disable the stop offline download button if it was stopped successfully
                        if (serviceAction == MapboxOfflineDownloaderService.SERVICE_ACTION.STOP_CURRENT_DOWNLOAD) {
                            currentMapDownload = null;
                           // setCanStopMapDownload(false);
                            displayToast("Download stopped");
                        } else {
                            if (!TextUtils.isEmpty(message)) {
                                // This is a download progress message
                                if (isValidDouble(message)) {
                                    if (Double.valueOf(message) == 100d) {
                                        currentMapDownload = null;
                                       // setCanStopMapDownload(false);
                                    } else {
                                       // setCanStopMapDownload(true);
                                    }
                                } else {
                                    displayToast(message);
                                }
                            }
                        }
                    }
                }
            } else {
                Log.i("KUJAKU SAMPLE APP TAG", "Broadcast message has null Extras");
            }

        }
    }

    private boolean isValidDouble(String doubleString) {
        String doubleRegex = "[+-]{0,1}[0-9]*.{0,1}[0-9]*";
        return (!doubleString.isEmpty() && doubleString.matches(doubleRegex));
    }
}
