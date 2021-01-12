package org.smartregister.reveal.widget;

import android.content.Context;
import android.view.View;
import android.widget.TextView;

import com.vijay.jsonwizard.constants.JsonFormConstants;
import com.vijay.jsonwizard.fragments.JsonFormFragment;
import com.vijay.jsonwizard.interfaces.CommonListener;
import com.vijay.jsonwizard.interfaces.JsonApi;
import com.vijay.jsonwizard.utils.ValidationStatus;
import com.vijay.jsonwizard.validators.edittext.RequiredValidator;
import com.vijay.jsonwizard.views.JsonFormFragmentView;
import com.vijay.jsonwizard.widgets.ToasterNotesFactory;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.List;

import static com.vijay.jsonwizard.constants.JsonFormConstants.KEY;
import static com.vijay.jsonwizard.constants.JsonFormConstants.V_REQUIRED;

/**
 * Created by samuelgithengi on 9/26/19.
 */
public class RevealToasterNotesFactory extends ToasterNotesFactory {

    public static ValidationStatus validate(JsonFormFragmentView formFragmentView,
                                            TextView textView) {
        View view = (View) textView.getParent().getParent();
        RequiredValidator validator = (RequiredValidator) textView.getTag(com.vijay.jsonwizard.R.id.v_required);
        if (validator != null && view.getVisibility() == View.VISIBLE) {
            return new ValidationStatus(false, validator.getErrorMessage(), formFragmentView, textView);
        }
        return new ValidationStatus(true, null, formFragmentView, textView);
    }

    @Override
    public List<View> getViewsFromJson(String stepName, Context context, JsonFormFragment formFragment, JSONObject jsonObject, CommonListener listener) throws Exception {
        return getViewsFromJson(stepName, context, formFragment, jsonObject, listener, false);
    }

    @Override
    public List<View> getViewsFromJson(String stepName, Context context, JsonFormFragment formFragment, JSONObject jsonObject, CommonListener listener, boolean popup) throws JSONException {
        List<View> views = super.getViewsFromJson(stepName, context, formFragment, jsonObject, listener, popup);
        View linearLayout = views.get(0);
        TextView textView = linearLayout.findViewById(com.vijay.jsonwizard.R.id.toaster_notes_text);
        textView.setTag(com.vijay.jsonwizard.R.id.address, stepName + ":" + jsonObject.getString(KEY));
        textView.setTag(com.vijay.jsonwizard.R.id.key, jsonObject.getString(KEY));
        addRequiredValidator(jsonObject, textView);
        formFragment.getJsonApi().addFormDataView(textView);
        return views;
    }

    private void addRequiredValidator(JSONObject jsonObject, TextView textView) throws JSONException {
        JSONObject requiredObject = jsonObject.optJSONObject(V_REQUIRED);
        if (requiredObject != null) {
            boolean requiredValue = requiredObject.getBoolean(JsonFormConstants.VALUE);
            if (Boolean.TRUE.equals(requiredValue)) {
                textView.setTag(com.vijay.jsonwizard.R.id.v_required, new RequiredValidator(requiredObject.optString(JsonFormConstants.ERR)));
            }
        }
    }
}
