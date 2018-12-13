package org.smartregister.reveal.activity;

import com.vijay.jsonwizard.activities.JsonFormActivity;
import com.vijay.jsonwizard.constants.JsonFormConstants;

import org.smartregister.reveal.R;
import org.smartregister.reveal.fragment.RevealJsonFormFragment;

/**
 * Created by samuelgithengi on 12/13/18.
 */
public class RevealJsonForm extends JsonFormActivity {

    @Override
    public void initializeFormFragment() {
        RevealJsonFormFragment revealJsonFormFragment = RevealJsonFormFragment.getFormFragment(JsonFormConstants.FIRST_STEP_NAME);
        getSupportFragmentManager().beginTransaction()
                .add(R.id.container, revealJsonFormFragment).commit();
    }
}
