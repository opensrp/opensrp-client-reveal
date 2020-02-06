package org.smartregister.reveal.task;

import android.content.Context;
import android.os.AsyncTask;

import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.OfflineMapHelper;

/**
 * Created by Richard Kareko on 2/4/20.
 */

public class FileHttpServerTask extends AsyncTask<Void, Void, Void> {
    private Context context;

    public FileHttpServerTask(Context context) {
        this.context = context;
    }

    @Override
    protected Void doInBackground(Void... params) {
        OfflineMapHelper.initializeFileHTTPServer(context, Constants.DG_ID_PLACEHOLDER);
        return null;
    }
}
