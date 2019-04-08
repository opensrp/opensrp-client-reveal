package org.smartregister.reveal.fragment;

import android.os.Bundle;
import android.support.v4.app.Fragment;

/**
 * Created by samuelgithengi on 4/8/19.
 */
public class StructureTasksFragment extends Fragment {

    public static StructureTasksFragment newInstance(Bundle bundle) {
        StructureTasksFragment fragment = new StructureTasksFragment();
        if (bundle != null) {
            fragment.setArguments(bundle);
        }
        return fragment;
    }
}
