package org.smartregister.reveal.widget;

import android.content.Context;
import android.view.View;

import com.vijay.jsonwizard.fragments.JsonFormFragment;
import com.vijay.jsonwizard.interfaces.CommonListener;
import com.vijay.jsonwizard.widgets.EditTextFactory;

import org.json.JSONObject;
import org.smartregister.reveal.R;

import java.util.List;

import static org.smartregister.reveal.util.Constants.JsonForm.NO_PADDING;

/**
 * Created by samuelgithengi on 12/17/18.
 */
public class RevealEditTextFactory extends EditTextFactory {

    private boolean hasNoPadding;

    @Override
    public List<View> getViewsFromJson(String stepName, Context context, JsonFormFragment formFragment, JSONObject jsonObject, CommonListener listener, boolean popup) throws Exception {
        hasNoPadding = new JSONObject(formFragment.getCurrentJsonState()).getJSONObject(stepName).optBoolean(NO_PADDING);
        return super.getViewsFromJson(stepName, context, formFragment, jsonObject, listener, popup);
    }

    @Override
    protected int getLayout() {
        if (hasNoPadding) {
            return R.layout.padded_item_edit_text;
        } else {
            return super.getLayout();
        }
    }
}
