package org.smartregister.reveal.activity;

import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;

import org.smartregister.reveal.view.RevealMapView;

/**
 * Created by samuelgithengi on 11/20/18.
 */
public abstract class BaseMapActivity extends AppCompatActivity {

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
}
