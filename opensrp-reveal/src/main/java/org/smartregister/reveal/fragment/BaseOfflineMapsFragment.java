package org.smartregister.reveal.fragment;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.content.LocalBroadcastManager;
import android.text.TextUtils;
import android.util.Log;
import android.widget.Toast;

import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.OfflineMapsFragmentContract;

import io.ona.kujaku.services.MapboxOfflineDownloaderService;
import io.ona.kujaku.utils.Constants;

public class BaseOfflineMapsFragment extends Fragment implements OfflineMapsFragmentContract.View {


    private static final String TAG = BaseOfflineMapsFragment.class.getName();
    private MapDownloadReceiver mapDownloadReceiver = new MapDownloadReceiver();

    protected String currentMapDownload;

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

    @Override
    public void displayToast(String message) {
        Toast.makeText(getContext(), message, Toast.LENGTH_LONG).show();
    }

    @Override
    public void displayError(int title, String message) {
        Toast.makeText(getContext(), message, Toast.LENGTH_LONG).show();
    }

    private class MapDownloadReceiver extends BroadcastReceiver {
        @Override
        public void onReceive(Context context, Intent intent) {
            Bundle bundle = intent.getExtras();
            if (bundle != null) {
                Log.i(TAG, intent.getExtras().toString());
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
                        } else if (serviceAction == MapboxOfflineDownloaderService.SERVICE_ACTION.DELETE_MAP) {
                            mapDeletedSuccessfully(mapUniqueName);
                        } else {
                            if (!TextUtils.isEmpty(message)) {
                                // This is a download progress message
                                if (isValidDouble(message)) {
                                    if (Double.valueOf(message) == 100d) {
                                        currentMapDownload = null;
                                        displayToast("Download finished successfuly");
                                        downloadCompleted(mapUniqueName);
                                        // setCanStopMapDownload(false);
                                    } else {
                                        // setCanStopMapDownload(true);
                                        displayToast("Download map for " + mapUniqueName + " in progress at " + Double.valueOf(message));
                                        downloadStarted(mapUniqueName);
                                    }
                                } else {
                                    displayToast(message);
                                }
                            }
                        }
                    }
                }
            } else {
                Log.i(TAG, "Broadcast message has null Extras");
            }

        }
    }

    protected boolean isValidDouble(String doubleString) {
        String doubleRegex = "[+-]{0,1}[0-9]*.{0,1}[0-9]*";
        return (!doubleString.isEmpty() && doubleString.matches(doubleRegex));
    }

    protected void downloadCompleted(String mapUniqueName) {
        // Implement in child class
    }

    protected void downloadStarted(String mapUniqueName) {
        // Implement in child class
    }

    protected void mapDeletedSuccessfully(String mapUniqueName) {
        // implement in child class
    }
}
