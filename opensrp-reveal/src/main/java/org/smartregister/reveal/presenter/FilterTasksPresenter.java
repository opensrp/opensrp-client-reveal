package org.smartregister.reveal.presenter;

import android.support.annotation.StringRes;

import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.FilterTasksContract;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.BusinessStatus;
import org.smartregister.reveal.util.Constants.Intervention;
import org.smartregister.reveal.util.Constants.InterventionType;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Created by samuelgithengi on 12/18/19.
 */
public class FilterTasksPresenter implements FilterTasksContract.Presenter {


    private FilterTasksContract.View view;

    private Map<String, Integer> labelsMap;

    private Map<String, Set<String>> checkedFilters = new HashMap<>();

    public FilterTasksPresenter(FilterTasksContract.View view) {
        this.view = view;
        populateLabels();
    }


    private void populateLabels() {

        labelsMap = new HashMap<>();
        //Interventions
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

        //Intervention Types
        labelsMap.put(InterventionType.OPERATIONAL_AREA, R.string.operational_area);
        labelsMap.put(InterventionType.STRUCTURE, R.string.structure);
        labelsMap.put(InterventionType.FAMILY, R.string.family);
        labelsMap.put(InterventionType.PERSON, R.string.person);

        //Business status
        labelsMap.put(BusinessStatus.NOT_VISITED, R.string.not_visited);
        labelsMap.put(BusinessStatus.NOT_SPRAYED, R.string.not_visited);
        labelsMap.put(BusinessStatus.NOT_SPRAYABLE, R.string.not_sprayable);
        labelsMap.put(BusinessStatus.SPRAYED, R.string.sprayed);

        labelsMap.put(BusinessStatus.COMPLETE, R.string.complete);
        labelsMap.put(BusinessStatus.INCOMPLETE, R.string.incomplete);
        labelsMap.put(BusinessStatus.NOT_ELIGIBLE, R.string.not_eligible);
        labelsMap.put(BusinessStatus.IN_PROGRESS, R.string.in_progress);
    }


    @Override
    public @StringRes
    Integer getStringResource(String intervention) {
        return labelsMap.get(intervention);
    }

    @Override
    public void onToggleChanged(int buttonId, Object filterCategory, Object filterKey) {
        if (filterCategory != null) {
            Set<String> selected = checkedFilters.get(filterCategory.toString());
            if (selected == null) {
                selected = new HashSet<>();
                checkedFilters.put(filterCategory.toString(), selected);
            }
            selected.add(filterKey != null ? filterKey.toString() : buttonId + "");
        }
        view.onFiltedSelected(checkedFilters.size());
    }
}
