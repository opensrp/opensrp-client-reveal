package org.smartregister.reveal.widget;

import android.content.Context;
import android.view.View;

import com.vijay.jsonwizard.fragments.JsonFormFragment;
import com.vijay.jsonwizard.interfaces.CommonListener;
import com.vijay.jsonwizard.widgets.NativeRadioButtonFactory;

import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.reveal.R;

import java.util.List;

import static org.smartregister.reveal.util.Constants.JsonForm.NO_PADDING;

/**
 * Created by samuelgithengi on 2/19/19.
 */
public class RevealRadioButtonFactory extends NativeRadioButtonFactory {

    @Override
    protected List<View> attachJson(String stepName, Context context, JsonFormFragment formFragment, JSONObject jsonObject, CommonListener listener, boolean popup) throws JSONException {
        boolean hasNoPadding = new JSONObject(formFragment.getCurrentJsonState()).getJSONObject(stepName).optBoolean(NO_PADDING);
        List<View> views = super.attachJson(stepName, context, formFragment, jsonObject, listener, popup);
        if (hasNoPadding) {
            views.get(0).setPaddingRelative(context.getResources().getDimensionPixelSize(R.dimen.default_left_margin),
                    0, context.getResources().getDimensionPixelSize(R.dimen.default_right_margin), 0);
        }
        return views;
    }

}
