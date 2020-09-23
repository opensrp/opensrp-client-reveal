package org.smartregister.reveal.view;

import android.content.Context;
import android.os.Bundle;

import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.util.Country;
import org.smartregister.util.LangUtils;
import org.smartregister.view.activity.MultiLanguageActivity;

/**
 * Created by samuelgithengi on 11/20/18.
 */
public abstract class BaseMapActivity extends MultiLanguageActivity {

    protected RevealMapView kujakuMapView;

    @Override
    protected void onStart() {
        super.onStart();
        if (kujakuMapView != null) {
            kujakuMapView.onStart();
        }
    }

    @Override
    protected void onResume() {
        super.onResume();
        if (kujakuMapView != null)
            kujakuMapView.onResume();
    }

    @Override
    protected void onPause() {
        super.onPause();
        if (kujakuMapView != null)
            kujakuMapView.onPause();
    }

    @Override
    protected void onStop() {
        super.onStop();
        if (kujakuMapView != null)
            kujakuMapView.onStop();
    }

    @Override
    protected void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        if (kujakuMapView != null)
            kujakuMapView.onSaveInstanceState(outState);
    }

    @Override
    public void onLowMemory() {
        super.onLowMemory();
        if (kujakuMapView != null)
            kujakuMapView.onLowMemory();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        if (kujakuMapView != null)
            kujakuMapView.onDestroy();
    }

    @Override
    protected void attachBaseContext(Context base) {
        if (BuildConfig.BUILD_COUNTRY == Country.THAILAND) {
            LangUtils.saveLanguage(base.getApplicationContext(), "th");
        } else {
            LangUtils.saveLanguage(base.getApplicationContext(), "en");
        }
        super.attachBaseContext(base);
    }
}
