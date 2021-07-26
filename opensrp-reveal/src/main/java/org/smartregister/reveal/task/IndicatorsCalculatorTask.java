package org.smartregister.reveal.task;

import android.app.Activity;
import android.os.AsyncTask;
import android.view.View;
import android.widget.TableLayout;
import android.widget.TextView;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.domain.Setting;
import org.smartregister.reporting.view.ProgressIndicatorView;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.model.IndicatorDetails;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.util.Constants.CONFIGURATION;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.util.IndicatorUtils;
import org.smartregister.reveal.util.Utils;
import org.smartregister.reveal.view.ListTasksActivity;

import java.util.Arrays;
import java.util.List;

import timber.log.Timber;

/**
 * Created by ndegwamartin on 2019-09-23.
 */
public class IndicatorsCalculatorTask extends AsyncTask<Void, Void, IndicatorDetails> {

    private ProgressIndicatorView progressIndicator;
    private ProgressIndicatorView progressIndicator2;
    private ProgressIndicatorView progressIndicator3;
    protected Activity activity;
    private List<TaskDetails> tasks;
    private TableLayout tableView;

    public IndicatorsCalculatorTask(Activity context, List<TaskDetails> tasks) {
        this.activity = context;
        this.tasks = tasks;

    }

    @Override
    protected void onPreExecute() {
        progressIndicator = activity.findViewById(R.id.progressIndicatorView);
        progressIndicator2 = activity.findViewById(R.id.progressIndicatorView2);
        progressIndicator3 = activity.findViewById(R.id.progressIndicatorView3);
        tableView = activity.findViewById(R.id.tempTableView);

    }

    @Override
    protected IndicatorDetails doInBackground(Void... params) {
        IndicatorDetails indicatorDetails = null;
        if(BuildConfig.BUILD_COUNTRY == Country.NIGERIA){
            indicatorDetails = IndicatorUtils.processIndicatorsNigeria(this.tasks);
        } else{
            indicatorDetails = IndicatorUtils.processIndicators(this.tasks);

        }
        if (BuildConfig.BUILD_COUNTRY == Country.ZAMBIA) {
            indicatorDetails.setSprayIndicatorList(IndicatorUtils.populateSprayIndicators(this.activity, indicatorDetails));
        } else if (BuildConfig.BUILD_COUNTRY == Country.NAMIBIA) {
            indicatorDetails.setTarget(calculateTarget());
            indicatorDetails.setSprayIndicatorList(IndicatorUtils.populateNamibiaSprayIndicators(this.activity, indicatorDetails));
        } else if(BuildConfig.BUILD_COUNTRY == Country.NIGERIA){
            indicatorDetails.setSprayIndicatorList(IndicatorUtils.populateNigeriaIndicators(this.activity,indicatorDetails));
        }
        return indicatorDetails;

    }

    public int calculateTarget() {
        int target = 0;
        String operationalId = Utils.getCurrentLocationId();
        Setting metadata = RevealApplication.getInstance().getSettingsRepository().getSetting(CONFIGURATION.JURISDICTION_METADATA);
        if (metadata != null) {
            try {
                JSONArray settings = new JSONObject(metadata.getValue()).getJSONArray(CONFIGURATION.SETTINGS);
                for (int i = 0; i < settings.length(); i++) {
                    JSONObject setting = settings.getJSONObject(i);
                    if (setting.getString(CONFIGURATION.KEY).equalsIgnoreCase(operationalId)) {
                        target = setting.getInt(CONFIGURATION.VALUE);
                        break;
                    }
                }
            } catch (JSONException e) {
                Timber.e(e);
            }
        }
        return target;
    }


    @Override
    protected void onPostExecute(IndicatorDetails indicatorDetails) {
        if (progressIndicator == null || indicatorDetails == null) {
            Timber.w("progress indicator or indicators is null");
            return;
        }
        if (BuildConfig.BUILD_COUNTRY == Country.ZAMBIA) {
            progressIndicator.setProgress(indicatorDetails.getProgress());
            progressIndicator.setTitle(this.activity.getString(R.string.n_percent, indicatorDetails.getProgress()));

            int totalFound = indicatorDetails.getSprayed() + indicatorDetails.getNotSprayed();
            int progress2 = indicatorDetails.getTotalStructures() > 0 ? Math.round(totalFound * 100 / indicatorDetails.getTotalStructures()) : 0;

            progressIndicator2.setProgress(progress2);
            progressIndicator2.setTitle(this.activity.getString(R.string.n_percent, progress2));

            int progress3 = totalFound > 0 ? Math.round((indicatorDetails.getSprayed() * 100 / totalFound)) : 0;

            progressIndicator3.setProgress(progress3);
            progressIndicator3.setTitle(this.activity.getString(R.string.n_percent, progress3));
        } else if (BuildConfig.BUILD_COUNTRY == Country.NAMIBIA) {

            progressIndicator3.setSubTitle(activity.getString(R.string.target_coverage));
            int targetCoverage = indicatorDetails.getTarget() == 0 ? 100 : (int) (indicatorDetails.getSprayed() * 100.0 / indicatorDetails.getTarget());
            progressIndicator3.setProgress(targetCoverage);
            progressIndicator3.setTitle(activity.getString(R.string.n_percent, targetCoverage));

            progressIndicator2.setSubTitle(activity.getString(R.string.found_coverage));
            int coverage = (int) (indicatorDetails.getSprayed() * 100.0 / indicatorDetails.getTotalStructures());
            progressIndicator2.setProgress(coverage);
            progressIndicator2.setTitle(this.activity.getString(R.string.n_percent, coverage));

            progressIndicator.setVisibility(View.GONE);
        } else if(BuildConfig.BUILD_COUNTRY == Country.NIGERIA){

            int totalStructures = indicatorDetails.getTotalStructures() - indicatorDetails.getIneligible();
            int  visited = totalStructures - indicatorDetails.getNotVisited();
            int foundCoverage = totalStructures > 0 ? Math.round(visited * 100 / totalStructures) : 0;
            progressIndicator.setSubTitle(this.activity.getString(R.string.found_coverage));
            progressIndicator.setProgress(foundCoverage);
            progressIndicator.setTitle(this.activity.getString(R.string.n_percent,foundCoverage));


            int distributionCoverage = indicatorDetails.getFoundStructures() > 0 ? Math.round(indicatorDetails.getCompleteDrugDistribution() * 100 / indicatorDetails.getFoundStructures()):0;
            progressIndicator2.setSubTitle(this.activity.getString(R.string.distribution_coverage));
            progressIndicator2.setProgress(distributionCoverage);
            progressIndicator2.setTitle(this.activity.getString(R.string.n_percent,distributionCoverage));


            int individualsComplete =  indicatorDetails.getChildrenEligible() > 0 ? Math.round(indicatorDetails.getTotalIndividualTreated()* 100 /indicatorDetails.getChildrenEligible()): 0;
            progressIndicator3.setSubTitle(this.activity.getString(R.string.and_individuals_complete));
            progressIndicator3.setProgress(individualsComplete);
            progressIndicator3.setTitle(this.activity.getString(R.string.n_percent,individualsComplete));

        }
        
        
        populateTableView(Arrays.asList(R.id.total_structures_cell,
                                        R.id.structures_visited_cell,
                                        R.id.structures_not_visited_cell,
                                        R.id.structure_confirmed_eligible_cell,
                                        R.id.structure_complete_drug_distribution_cell,
                                        R.id.structure_partial_drug_distribution_cell,
                                        R.id.individual_total_number_of_children_eligible_3_to_49_mos_cell,
                                        R.id.individual_total_treated_3_to_59_mos_cell),indicatorDetails.getSprayIndicatorList());
        //Show or hide depending on plan

        ((View) progressIndicator.getParent()).setVisibility(Utils.getInterventionLabel() == R.string.irs || Country.NIGERIA == BuildConfig.BUILD_COUNTRY ? View.VISIBLE : View.GONE);

        if (activity instanceof ListTasksActivity)
            ((ListTasksActivity) activity).positionMyLocationAndLayerSwitcher();
    }


    private void populateTableView(List<Integer> cellResourceIdentifiers,List<String> sprayIndicatorList){
        for(Integer resourceId : cellResourceIdentifiers){
            TextView textView = tableView.findViewById(resourceId);
            textView.setText(sprayIndicatorList.get(cellResourceIdentifiers.indexOf(resourceId) * 2 + 1));
        }
    }
}
