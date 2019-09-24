package org.smartregister.reveal.task;

import android.app.Activity;
import android.os.AsyncTask;

import org.smartregister.reporting.view.ProgressIndicatorView;
import org.smartregister.reveal.R;
import org.smartregister.reveal.model.TaskDetails;
import org.smartregister.reveal.util.Constants;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by ndegwamartin on 2019-09-23.
 */
public class IndicatorsCalculatorTask extends AsyncTask<Void, Void, Map<String, Integer>> {

    private ProgressIndicatorView progressIndicator;
    private ProgressIndicatorView progressIndicator2;
    private ProgressIndicatorView progressIndicator3;
    private Activity activity;
    private List<TaskDetails> tasks;

    public IndicatorsCalculatorTask(Activity context, List<TaskDetails> tasks) {
        this.activity = context;
        this.tasks = tasks;

    }

    @Override
    protected void onPreExecute() {
        progressIndicator = activity.findViewById(R.id.progressIndicatorView);
        progressIndicator2 = activity.findViewById(R.id.progressIndicatorView2);
        progressIndicator3 = activity.findViewById(R.id.progressIndicatorView3);
    }

    @Override
    protected Map<String, Integer> doInBackground(Void... params) {

        Map<String, Integer> map = new HashMap<>();

        for (int i = 0; i < this.tasks.size(); i++) {

            String key = this.tasks.get(i).getSprayStatus() == null ? Constants.NULL_KEY : this.tasks.get(i).getSprayStatus();

            int newValue = map.get(key) != null ? map.get(key) + 1 : 1;

            map.put(key, newValue);

        }
        return map;
    }

    @Override
    protected void onPostExecute(Map<String, Integer> map) {

        Integer sprayedValue = map.get(Constants.BusinessStatus.SPRAYED);
        Integer notSprayedValue = map.get(Constants.BusinessStatus.NOT_SPRAYED);

        int sprayed = sprayedValue != null ? sprayedValue : 0;
        int notSprayed = notSprayedValue != null ? notSprayedValue : 0;
        int totalStructures = this.tasks.size();
        int progress = totalStructures > 0 ? Math.round(sprayed * 100 / totalStructures) : 0;

        progressIndicator.setProgress(progress);
        progressIndicator.setTitle(progress + "%");

        int totalFound = (sprayed + notSprayed);
        int progress2 = totalStructures > 0 ? Math.round(totalFound * 100 / totalStructures) : 0;

        progressIndicator2.setProgress(progress2);
        progressIndicator2.setTitle(progress2 + "%");

        int progress3 = totalFound > 0 ? Math.round((sprayed * 100 / totalFound)) : 0;

        progressIndicator3.setProgress(progress3);
        progressIndicator3.setTitle(progress3 + "%");

    }
}
