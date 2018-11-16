package org.smartregister.reveal.application;


import org.smartregister.reveal.R;

/**
 * Created by ndegwamartin on 27/05/2018.
 */

public class TestRevealcApplication extends RevealApplication {
    @Override
    public void onCreate() {
        super.onCreate();
        setTheme(R.style.Theme_AppCompat); //or just R.style.Theme_AppCompat
    }
}
