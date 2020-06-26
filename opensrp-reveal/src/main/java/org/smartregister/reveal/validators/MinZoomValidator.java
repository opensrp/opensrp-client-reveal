package org.smartregister.reveal.validators;

import androidx.annotation.NonNull;

import com.vijay.jsonwizard.validators.edittext.MinNumericValidator;

/**
 * Created by samuelgithengi on 2/4/19.
 */
public class MinZoomValidator extends MinNumericValidator {
    public MinZoomValidator(@NonNull String errorMessage, @NonNull double minValue) {
        super(errorMessage, minValue);
    }
}
