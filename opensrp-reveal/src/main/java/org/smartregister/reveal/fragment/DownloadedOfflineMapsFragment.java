package org.smartregister.reveal.fragment;

import android.os.Bundle;
import android.support.v4.app.Fragment;

public class DownloadedOfflineMapsFragment extends Fragment {

    public static DownloadedOfflineMapsFragment newInstance(Bundle bundle) {

        DownloadedOfflineMapsFragment fragment = new DownloadedOfflineMapsFragment();
        if (bundle != null) {
            fragment.setArguments(bundle);
        }
        return fragment;
    }
}
