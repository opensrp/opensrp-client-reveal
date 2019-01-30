package org.smartregister.reveal.presenter;

import android.view.View;
import android.widget.LinearLayout;

import com.vijay.jsonwizard.fragments.JsonFormFragment;
import com.vijay.jsonwizard.interactors.JsonFormInteractor;
import com.vijay.jsonwizard.presenters.JsonFormFragmentPresenter;
import com.vijay.jsonwizard.utils.ValidationStatus;

import org.smartregister.reveal.view.RevealMapView;
import org.smartregister.reveal.widget.GeoWidgetFactory;

/**
 * Created by samuelgithengi on 1/30/19.
 */
public class RevealJsonFormFragmentPresenter extends JsonFormFragmentPresenter {

    private JsonFormFragment formFragment;

    public RevealJsonFormFragmentPresenter(JsonFormFragment formFragment, JsonFormInteractor jsonFormInteractor) {
        super(formFragment, jsonFormInteractor);
        this.formFragment = formFragment;
    }

    @Override
    public ValidationStatus writeValuesAndValidate(LinearLayout mainView) {
        ValidationStatus validationStatus = super.writeValuesAndValidate(mainView);
        if (validationStatus.isValid()) {
            for (View childAt : formFragment.getJsonApi().getFormDataViews()) {
                if (childAt instanceof RevealMapView) {
                    RevealMapView mapView = (RevealMapView) childAt;
                    validationStatus = GeoWidgetFactory.validate(formFragment, mapView);
                }
            }

        }
        return validationStatus;
    }

}
