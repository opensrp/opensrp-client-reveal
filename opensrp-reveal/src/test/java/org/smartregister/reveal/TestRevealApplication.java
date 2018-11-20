package org.smartregister.reveal;


import org.smartregister.reveal.application.RevealApplication;


public class TestRevealApplication extends RevealApplication {
    @Override
    public void onCreate() {
        super.onCreate();
        setTheme(R.style.Theme_AppCompat); //or just R.style.Theme_AppCompat
    }
}
