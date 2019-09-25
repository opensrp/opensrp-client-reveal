package org.smartregister.reveal.task;

import android.app.Activity;
import android.os.AsyncTask;

import org.smartregister.reporting.view.ProgressIndicatorView;
import org.smartregister.reporting.view.TableView;
import org.smartregister.reveal.R;
import org.smartregister.reveal.model.IndicatorDetails;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.util.Constants;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Created by ndegwamartin on 2019-09-23.
 */
public class IndicatorsCalculatorTask extends AsyncTask<Void, Void, IndicatorDetails> {

    private ProgressIndicatorView progressIndicator;
    private ProgressIndicatorView progressIndicator2;
    private ProgressIndicatorView progressIndicator3;
    private Activity activity;
    private List<TaskDetails> tasks;
    TableView tableView;

    public IndicatorsCalculatorTask(Activity context, List<TaskDetails> tasks) {
        this.activity = context;
        this.tasks = tasks;

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

        IndicatorDetails indicatorDetails = new IndicatorDetails();

        for (int i = 0; i < this.tasks.size(); i++) {


            if (Constants.BusinessStatus.SPRAYED.equals(this.tasks.get(i).getSprayStatus())) {

                indicatorDetails.setSprayed(indicatorDetails.getSprayed() + 1);

            } else if (Constants.BusinessStatus.NOT_SPRAYED.equals(this.tasks.get(i).getSprayStatus())) {

                indicatorDetails.setNotSprayed(indicatorDetails.getNotSprayed() + 1);

            } else {

                indicatorDetails.setNotVisited(indicatorDetails.getNotVisited() + 1);
            }

        }

        int totalStructures = this.tasks.size();
        int progress = totalStructures > 0 ? Math.round(indicatorDetails.getSprayed() * 100 / totalStructures) : 0;


        indicatorDetails.setTotalStructures(totalStructures);
        indicatorDetails.setProgress(progress);

        List<String> sprayIndicator = new ArrayList<>();

        sprayIndicator.add(this.activity.getResources().getString(R.string.spray_coverage));
        sprayIndicator.add(this.activity.getResources().getString(R.string.n_percent, progress));

        int totalFound = (indicatorDetails.getSprayed() + indicatorDetails.getNotSprayed());

        sprayIndicator.add(this.activity.getResources().getString(R.string.structures_remaining_90));
        sprayIndicator.add(String.valueOf(Math.round(totalStructures * 0.9) - indicatorDetails.getSprayed()));


        sprayIndicator.add(this.activity.getResources().getString(R.string.total_structures));
        sprayIndicator.add(String.valueOf(totalStructures));


        sprayIndicator.add(this.activity.getResources().getString(R.string.structures_not_visited));
        sprayIndicator.add(String.valueOf(indicatorDetails.getNotVisited()));


        sprayIndicator.add(this.activity.getResources().getString(R.string.structures_visited_found));
        sprayIndicator.add(String.valueOf(totalFound));


        sprayIndicator.add(this.activity.getResources().getString(R.string.sprayed));
        sprayIndicator.add(String.valueOf(indicatorDetails.getSprayed()));

        sprayIndicator.add(this.activity.getResources().getString(R.string.structures_not_sprayed));
        sprayIndicator.add(String.valueOf(indicatorDetails.getNotSprayed()));

        indicatorDetails.setSprayIndicatorList(sprayIndicator);

        return indicatorDetails;
    }

    @Override
    protected void onPostExecute(IndicatorDetails indicatorDetails) {


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

    }
}
