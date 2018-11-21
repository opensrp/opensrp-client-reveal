package org.smartregister.reveal.activity;

import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;

import io.ona.kujaku.views.KujakuMapView;

/**
 * Created by samuelgithengi on 11/20/18.
 */
public abstract class BaseMapActivity extends AppCompatActivity {

    protected KujakuMapView kujakuMapView;

    @Override
    protected void onStart() {
        super.onStart();
        kujakuMapView.onStart();
    }

    @Override
    protected void onResume() {
        super.onResume();
        kujakuMapView.onResume();
    }

    @Override
    protected void onPause() {
        super.onPause();
        kujakuMapView.onPause();
    }

    @Override
    protected void onStop() {
        super.onStop();
        kujakuMapView.onStop();
    }

    @Override
    protected void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        kujakuMapView.onSaveInstanceState(outState);
    }

    @Override
    public void onLowMemory() {
        super.onLowMemory();
        kujakuMapView.onLowMemory();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        kujakuMapView.onDestroy();
    }
}
