package org.smartregister.reveal;


import org.smartregister.Context;
import org.smartregister.CoreLibrary;
import org.smartregister.configurableviews.ConfigurableViewsLibrary;
import org.smartregister.family.FamilyLibrary;
import org.smartregister.repository.Repository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.AppExecutors;

import java.util.concurrent.Executors;

import static org.mockito.Mockito.mock;


public class TestRevealApplication extends RevealApplication {
    @Override
    public void onCreate() {
        mInstance = this;
        context = Context.getInstance();
        context.updateApplicationContext(getApplicationContext());
        CoreLibrary.init(context);
        ConfigurableViewsLibrary.init(context, getRepository());

        FamilyLibrary.init(context, getRepository(), getMetadata(), BuildConfig.VERSION_CODE, BuildConfig.DATABASE_VERSION);

        setTheme(R.style.Theme_AppCompat); //or just R.style.Theme_AppCompat
    }

    @Override
    public AppExecutors getAppExecutors() {
        return new AppExecutors(Executors.newSingleThreadExecutor(), Executors.newSingleThreadExecutor(), Executors.newSingleThreadExecutor());
    }

    @Override
    public Repository getRepository() {
        return mock(Repository.class);
    }
}
