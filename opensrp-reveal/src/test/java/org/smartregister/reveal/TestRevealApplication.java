package org.smartregister.reveal;


import com.vijay.jsonwizard.activities.JsonWizardFormActivity;

import org.smartregister.Context;
import org.smartregister.CoreLibrary;
import org.smartregister.configurableviews.ConfigurableViewsLibrary;
import org.smartregister.family.FamilyLibrary;
import org.smartregister.family.activity.FamilyWizardFormActivity;
import org.smartregister.family.domain.FamilyMetadata;
import org.smartregister.repository.Repository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.FamilyConstants;
import org.smartregister.reveal.view.FamilyProfileActivity;

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

    @Override
    public FamilyMetadata getMetadata() {
        // This method is overriden so that default family forms are loaded
        FamilyMetadata metadata = new FamilyMetadata(FamilyWizardFormActivity.class, JsonWizardFormActivity.class, FamilyProfileActivity.class, FamilyConstants.CONFIGURATION.UNIQUE_ID_KEY, true);
        metadata.updateFamilyRegister(FamilyConstants.JSON_FORM.FAMILY_REGISTER, FamilyConstants.TABLE_NAME.FAMILY, FamilyConstants.EventType.FAMILY_REGISTRATION, FamilyConstants.EventType.UPDATE_FAMILY_REGISTRATION, FamilyConstants.CONFIGURATION.FAMILY_REGISTER, FamilyConstants.RELATIONSHIP.FAMILY_HEAD, FamilyConstants.RELATIONSHIP.PRIMARY_CAREGIVER);
        metadata.updateFamilyMemberRegister(FamilyConstants.JSON_FORM.FAMILY_MEMBER_REGISTER, FamilyConstants.TABLE_NAME.FAMILY_MEMBER, FamilyConstants.EventType.FAMILY_MEMBER_REGISTRATION, FamilyConstants.EventType.UPDATE_FAMILY_MEMBER_REGISTRATION, FamilyConstants.CONFIGURATION.FAMILY_MEMBER_REGISTER, FamilyConstants.RELATIONSHIP.FAMILY);
        metadata.updateFamilyDueRegister(FamilyConstants.TABLE_NAME.FAMILY_MEMBER, 20, true);
        metadata.updateFamilyActivityRegister(FamilyConstants.TABLE_NAME.FAMILY_MEMBER, Integer.MAX_VALUE, false);
        metadata.updateFamilyOtherMemberRegister(FamilyConstants.TABLE_NAME.FAMILY_MEMBER, Integer.MAX_VALUE, false);
        return metadata;
    }
}
