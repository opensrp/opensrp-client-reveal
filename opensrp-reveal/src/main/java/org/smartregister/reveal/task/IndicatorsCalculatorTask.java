package org.smartregister.reveal.task;

import android.app.Activity;
import android.os.AsyncTask;
import android.view.View;

import net.sqlcipher.database.SQLiteDatabase;

import org.smartregister.domain.Location;
import org.smartregister.reporting.view.ProgressIndicatorView;
import org.smartregister.reporting.view.TableView;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.model.IndicatorDetails;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.util.IndicatorUtils;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.Utils;
import org.smartregister.reveal.view.ListTasksActivity;

import java.util.Arrays;
import java.util.List;

import timber.log.Timber;

/**
 * Created by ndegwamartin on 2019-09-23.
 */
public class IndicatorsCalculatorTask extends AsyncTask<Void, Void, IndicatorDetails> {

    private final PreferencesUtil prefsUtil;
    private final SQLiteDatabase sqLiteDatabase;
    private ProgressIndicatorView progressIndicator;
    private ProgressIndicatorView progressIndicator2;
    private ProgressIndicatorView progressIndicator3;
    protected Activity activity;
    private List<TaskDetails> tasks;
    private TableView tableView;

    public IndicatorsCalculatorTask(Activity context, List<TaskDetails> tasks) {
        this.activity = context;
        this.tasks = tasks;
        prefsUtil = PreferencesUtil.getInstance();
        sqLiteDatabase = RevealApplication.getInstance().getRepository().getReadableDatabase();

    }

    @Override
    protected void onPreExecute() {
        progressIndicator = activity.findViewById(R.id.progressIndicatorView);
        progressIndicator2 = activity.findViewById(R.id.progressIndicatorView2);
        progressIndicator3 = activity.findViewById(R.id.progressIndicatorView3);
        tableView = activity.findViewById(R.id.tableView);

    }

    @Override
    protected IndicatorDetails doInBackground(Void... params) {
        IndicatorDetails indicatorDetails = null;
        if (BuildConfig.BUILD_COUNTRY == Country.ZAMBIA) {
            indicatorDetails = IndicatorUtils.processIndicators(this.tasks);
        } else if (BuildConfig.BUILD_COUNTRY == Country.NAMIBIA) {
            Location operationalArea = Utils.getOperationalAreaLocation(prefsUtil.getCurrentOperationalArea());
            indicatorDetails = IndicatorUtils.getNamibiaIndicators(operationalArea.getId(), sqLiteDatabase);
        }
        if (indicatorDetails != null) {
            List<String> sprayIndicator = IndicatorUtils.populateSprayIndicators(this.activity, indicatorDetails);
            indicatorDetails.setSprayIndicatorList(sprayIndicator);
        }

        return indicatorDetails;

    }


    @Override
    protected void onPostExecute(IndicatorDetails indicatorDetails) {

        if (progressIndicator == null || indicatorDetails == null) {
            Timber.w("progress indicator or indicators is null");
            return;
        }
        progressIndicator.setProgress(indicatorDetails.getProgress());
        progressIndicator.setTitle(this.activity.getString(R.string.n_percent, indicatorDetails.getProgress()));

        int totalFound = indicatorDetails.getSprayed() + indicatorDetails.getNotSprayed();
        int progress2 = indicatorDetails.getTotalStructures() > 0 ? Math.round(totalFound * 100 / indicatorDetails.getTotalStructures()) : 0;

        progressIndicator2.setProgress(progress2);
        progressIndicator2.setTitle(this.activity.getString(R.string.n_percent, progress2));

        int progress3 = totalFound > 0 ? Math.round((indicatorDetails.getSprayed() * 100 / totalFound)) : 0;

        progressIndicator3.setProgress(progress3);
        progressIndicator3.setTitle(this.activity.getString(R.string.n_percent, progress3));

        tableView.setTableData(Arrays.asList(new String[]{this.activity.getString(R.string.indicator), this.activity.getString(R.string.value)}), indicatorDetails.getSprayIndicatorList());

        //Show or hide depending on plan

        ((View) progressIndicator.getParent()).setVisibility(org.smartregister.reveal.util.Utils.getInterventionLabel() == R.string.irs ? View.VISIBLE : View.GONE);

        if (activity instanceof ListTasksActivity)
            ((ListTasksActivity) activity).positionMyLocationAndLayerSwitcher();
    }


}
