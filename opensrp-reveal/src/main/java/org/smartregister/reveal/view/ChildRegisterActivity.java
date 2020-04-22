package org.smartregister.reveal.view;

import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;

import org.smartregister.reveal.R;
import org.smartregister.reveal.fragment.ChildRegisterFragment;
import org.smartregister.view.activity.SecuredActivity;

public class ChildRegisterActivity extends SecuredActivity {
    public static final String DISPLAY_FRAGMENT = "DISPLAY_FRAGMENT";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.child_register_activity);


        Bundle bundle = getIntent().getExtras();
        if (bundle != null) {
            String fragmentName = bundle.getString(DISPLAY_FRAGMENT);
            Fragment fragment = getDestinationFragment(fragmentName);
            if (fragment != null)
                switchToFragment(fragment);
        }else{
            switchToFragment(getDestinationFragment(ChildRegisterFragment.TAG));
        }
    }

    private void switchToFragment(Fragment fragment) {
        getSupportFragmentManager().beginTransaction()
                .add(R.id.content, fragment)
                .commit();
    }

    @Nullable
    private Fragment getDestinationFragment(@Nullable String destinationFragment) {
        if (destinationFragment == null)
            return null;

        switch (destinationFragment) {
            default:
                return new ChildRegisterFragment();
        }
    }

    @Override
    protected void onCreation() {

    }

    @Override
    protected void onResumption() {

    }
}
