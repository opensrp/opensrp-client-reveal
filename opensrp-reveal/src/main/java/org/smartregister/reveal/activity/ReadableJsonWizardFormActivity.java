package org.smartregister.reveal.activity;

import com.vijay.jsonwizard.activities.JsonWizardFormActivity;
import com.vijay.jsonwizard.constants.JsonFormConstants;

import org.smartregister.reveal.R;
import org.smartregister.reveal.fragment.ReadableJsonWizardFormFragment;
import org.smartregister.reveal.util.Constants;

public class ReadableJsonWizardFormActivity extends JsonWizardFormActivity {


    @Override
    protected void initializeFormFragmentCore() {
        boolean readOnly = this.getIntent().getBooleanExtra(Constants.READ_ONLY, false);

        ReadableJsonWizardFormFragment jsonWizardFormFragment = ReadableJsonWizardFormFragment.getFormFragment(JsonFormConstants.STEP1, readOnly);

        getSupportFragmentManager().beginTransaction()
                .add(R.id.container, jsonWizardFormFragment).commit();
    }
}
