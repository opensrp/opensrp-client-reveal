package org.smartregister.reveal.widget;

import android.content.Context;
import android.support.constraint.ConstraintLayout;
import android.view.View;

import com.vijay.jsonwizard.fragments.JsonFormFragment;
import com.vijay.jsonwizard.interfaces.CommonListener;
import com.vijay.jsonwizard.utils.FormUtils;
import com.vijay.jsonwizard.views.CustomTextView;
import com.vijay.jsonwizard.widgets.LabelFactory;

import org.json.JSONArray;
import org.json.JSONObject;
import org.smartregister.reveal.R;

import java.util.List;

import static org.smartregister.reveal.util.Constants.JsonForm.NO_PADDING;

public class RevealLabelFactory extends LabelFactory {

    @Override
    public List<View> getViewsFromJson(String stepName, Context context, JsonFormFragment formFragment, JSONObject jsonObject, CommonListener listener, boolean popup) throws Exception {
        boolean hasNoPadding = new JSONObject(formFragment.getCurrentJsonState()).getJSONObject(stepName).optBoolean(NO_PADDING);
        List<View> views = super.getViewsFromJson(stepName, context, formFragment, jsonObject, listener, popup);
        if (hasNoPadding) {
            views.get(0).setPaddingRelative(context.getResources().getDimensionPixelSize(R.dimen.default_left_margin),
                    0, context.getResources().getDimensionPixelSize(R.dimen.default_right_margin), 0);
        }
        JSONArray canvasIds = new JSONArray();
        ConstraintLayout constraintLayout = FormUtils.createLabelLinearLayout(stepName, canvasIds, jsonObject, context, listener);
        CustomTextView labelText = (CustomTextView)constraintLayout.findViewById(R.id.label_text);
        labelText.setTextColor(context.getResources().getColor(R.color.black));
        views.add(constraintLayout);
        return views;
    }
}
