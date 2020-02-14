package org.smartregister.reveal.shadow;

import android.os.AsyncTask;

import org.robolectric.annotation.Implementation;
import org.robolectric.annotation.Implements;

/**
 * Created by Richard Kareko on 2/5/20.
 */

@Implements(AsyncTask.class)
public class AsyncTaskShadow {

    @Implementation
    protected Void doInBackground(Void... params) {
        // Do nothing
        return null;
    }
}
