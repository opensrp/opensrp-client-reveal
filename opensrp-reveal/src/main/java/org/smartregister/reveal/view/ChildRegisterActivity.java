package org.smartregister.reveal.view;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.view.Menu;

import org.smartregister.reveal.R;
import org.smartregister.reveal.fragment.ChildFilterFragment;
import org.smartregister.reveal.fragment.ChildRegisterFragment;
import org.smartregister.view.activity.SecuredActivity;

public class ChildRegisterActivity extends SecuredActivity {
    public static final String DISPLAY_FRAGMENT = "DISPLAY_FRAGMENT";

    public static void startFragment(Activity activity, String fragmentName, Bundle bundle, boolean clearStack) {
        Intent intent = new Intent(activity, ChildRegisterActivity.class);
        intent.putExtra(DISPLAY_FRAGMENT, fragmentName);

        if (bundle != null)
            intent.putExtras(bundle);
        if (clearStack)
            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);

        activity.startActivity(intent);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.child_register_activity);

        Bundle bundle = getIntent().getExtras();
        if (bundle != null) {
            String fragmentName = bundle.getString(DISPLAY_FRAGMENT);
            Fragment fragment = getDestinationFragment(fragmentName);
            if (fragment != null) {
                fragment.setArguments(bundle);
                switchToFragment(fragment);
            }
        } else {
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
            case ChildFilterFragment
                    .TAG:
                return new ChildFilterFragment();
            default:
                return new ChildRegisterFragment();
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        return false;
    }

    @Override
    protected void onCreation() {
        // do nothing
    }

    @Override
    protected void onResumption() {
        // do nothing
    }
}
