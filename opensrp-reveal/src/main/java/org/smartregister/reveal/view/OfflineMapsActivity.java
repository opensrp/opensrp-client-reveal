package org.smartregister.reveal.view;

import androidx.annotation.NonNull;

import org.smartregister.reveal.R;

public class OfflineMapsActivity extends org.smartregister.tasking.activity.OfflineMapsActivity {


    @NonNull
    @Override
    public String getMapStyleAssetPath() {
        return getString(R.string.reveal_offline_map_download_style);
    }
}
