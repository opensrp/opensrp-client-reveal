package org.smartregister.reveal.task;

import android.content.Context;
import android.os.AsyncTask;

import org.smartregister.reveal.R;
import org.smartregister.reveal.server.JavaHTTPServer;

import static org.smartregister.reveal.util.Constants.DG_ID_PLACEHOLDER;

/**
 * Created by Richard Kareko on 2/4/20.
 */

public class JavaHttpServerTask extends AsyncTask<Void, Void, Void> {
    Context context;

    public JavaHttpServerTask(Context context) {
        this.context = context;
    }

    @Override
    protected void onPreExecute() {
        JavaHTTPServer.styleJsonFile = context.getString(R.string.reveal_offline_map_download_style);
        JavaHTTPServer.digitalGlobeIdPlaceHolder = DG_ID_PLACEHOLDER;
    }

    @Override
    protected Void doInBackground(Void... params) {
        String[] args = {"test"};
        JavaHTTPServer.main(args);
        return null;
    }
}
