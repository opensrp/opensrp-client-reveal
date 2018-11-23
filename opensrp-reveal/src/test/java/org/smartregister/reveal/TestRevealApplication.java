package org.smartregister.reveal;


import org.smartregister.Context;
import org.smartregister.CoreLibrary;
import org.smartregister.configurableviews.ConfigurableViewsLibrary;
import org.smartregister.reveal.application.RevealApplication;


public class TestRevealApplication extends RevealApplication {
    @Override
    public void onCreate() {
        mInstance = this;
        context = Context.getInstance();
        context.updateApplicationContext(getApplicationContext());
        CoreLibrary.init(context);
        ConfigurableViewsLibrary.init(context, getRepository());

        setTheme(R.style.Theme_AppCompat); //or just R.style.Theme_AppCompat
    }
}
