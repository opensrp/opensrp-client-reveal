package org.smartregister.reveal.view;

import android.content.Intent;
import android.view.View;

import androidx.annotation.NonNull;

import com.mapbox.geojson.Feature;

import org.json.JSONArray;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.domain.Task;
import org.smartregister.family.presenter.BaseFamilyRegisterPresenter;
import org.smartregister.reveal.R;
import org.smartregister.reveal.fragment.EventRegisterFragment;
import org.smartregister.reveal.presenter.BaseRegisterPresenter;
import org.smartregister.reveal.presenter.TaskRegisterPresenter;
import org.smartregister.reveal.util.Constants;
import org.smartregister.view.fragment.BaseRegisterFragment;

import java.util.Collections;
import java.util.List;

/**
 * Created by samuelgithengi on 7/30/20.
 */
public class EventRegisterActivity extends BaseRevealRegisterActivity {


    @Override
    protected void initializePresenter() {
        presenter = new BaseRegisterPresenter(this);
    }

    @Override
    protected BaseRegisterFragment getRegisterFragment() {
        return new EventRegisterFragment();
    }

    @Override
    protected void onActivityResultExtended(int requestCode, int resultCode, Intent data) {

    }

    @Override
    public List<String> getViewIdentifiers() {
        return Collections.singletonList(Constants.EventsRegister.VIEW_IDENTIFIER);
    }

    @Override
    protected void registerBottomNavigation() {
        bottomNavigationView = findViewById(R.id.bottom_navigation);
        bottomNavigationView.setVisibility(View.GONE);
    }
}
