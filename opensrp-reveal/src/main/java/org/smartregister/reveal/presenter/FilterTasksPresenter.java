package org.smartregister.reveal.presenter;

import android.support.annotation.StringRes;

import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.FilterTasksContract;
import org.smartregister.reveal.util.Constants.Intervention;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by samuelgithengi on 12/18/19.
 */
public class FilterTasksPresenter implements FilterTasksContract.Presenter {


    private FilterTasksContract.View view;

    private Map<String, Integer> labelsMap;

    public FilterTasksPresenter(FilterTasksContract.View view) {
        this.view = view;
        populateLabels();
    }


    private void populateLabels() {

        labelsMap = new HashMap<>();
        labelsMap.put(Intervention.IRS, R.string.irs);
        labelsMap.put(Intervention.MOSQUITO_COLLECTION, R.string.mosquito_collection);
        labelsMap.put(Intervention.LARVAL_DIPPING, R.string.larval_dipping);
        labelsMap.put(Intervention.BCC, R.string.bcc_code);
        labelsMap.put(Intervention.BEDNET_DISTRIBUTION, R.string.bednet_distribution);
        labelsMap.put(Intervention.BLOOD_SCREENING, R.string.blood_screening);
        labelsMap.put(Intervention.CASE_CONFIRMATION, R.string.case_confirmation);
        labelsMap.put(Intervention.REGISTER_FAMILY, R.string.register_family);
        labelsMap.put(Intervention.PAOT, R.string.paot);
        labelsMap.put(Intervention.MDA_DISPENSE, R.string.mda_dispense);
        labelsMap.put(Intervention.MDA_ADHERENCE, R.string.mda_adherence);
        labelsMap.put(Intervention.IRS_VERIFICATION, R.string.irs_verification);
    }


    @Override
    public @StringRes
    Integer getStringResource(String intervention) {
        return labelsMap.get(intervention);
    }
}
