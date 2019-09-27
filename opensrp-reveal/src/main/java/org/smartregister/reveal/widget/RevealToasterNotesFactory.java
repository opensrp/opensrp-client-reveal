package org.smartregister.reveal.widget;

import android.content.Context;
import android.view.View;
import android.widget.LinearLayout;
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

import timber.log.Timber;

import static com.vijay.jsonwizard.constants.JsonFormConstants.KEY;

/**
 * Created by samuelgithengi on 9/26/19.
 */
public class RevealToasterNotesFactory extends ToasterNotesFactory {

    public static ValidationStatus validate(JsonFormFragmentView formFragmentView,
                                            TextView textView) {
        View view = (View) textView.getTag();
        RequiredValidator validator = (RequiredValidator) textView.getTag(com.vijay.jsonwizard.R.id.v_required);
        if (validator != null) {

            Timber.d("%s %s, %s", textView.isShown(), view.getVisibility() == View.VISIBLE, validator.getErrorMessage());
        }
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
        ((JsonApi) context).addFormDataView(textView);
        textView.setTag(linearLayout);
        linearLayout.setOnSystemUiVisibilityChangeListener(new View.OnSystemUiVisibilityChangeListener() {
            @Override
            public void onSystemUiVisibilityChange(int visibility) {
                Timber.d("onSystemUiVisibilityChange");
            }
        });

        Timber.d("%s, %s", linearLayout.getVisibility() == View.VISIBLE, jsonObject.getString(KEY));
        return views;
    }

    private void addRequiredValidator(JSONObject jsonObject, TextView textView) throws JSONException {
        JSONObject requiredObject = jsonObject.optJSONObject("v_required_1");
        if (requiredObject != null) {
            boolean requiredValue = requiredObject.getBoolean(JsonFormConstants.VALUE);
            if (Boolean.TRUE.equals(requiredValue)) {
                textView.setTag(com.vijay.jsonwizard.R.id.v_required, new RequiredValidator(requiredObject.getString(JsonFormConstants.ERR)));
            }
        }
    }
}
