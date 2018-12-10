package org.smartregister.reveal.job;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.util.Log;

import com.evernote.android.job.Job;
import com.evernote.android.job.JobCreator;

import org.smartregister.job.CampaignServiceJob;
import org.smartregister.job.LocationStructureServiceJob;
import org.smartregister.job.SyncServiceJob;
import org.smartregister.job.SyncTaskServiceJob;
import org.smartregister.sync.intent.SyncIntentService;

/**
 * Created by samuelgithengi on 11/21/18.
 */
public class RevealJobCreator implements JobCreator {
    @Nullable
    @Override
    public Job create(@NonNull String tag) {
        switch (tag) {
            case SyncServiceJob.TAG:
                return new SyncServiceJob(SyncIntentService.class);
            case CampaignServiceJob.TAG:
                return new CampaignServiceJob();
            case SyncTaskServiceJob.TAG:
                return new SyncTaskServiceJob();
            case LocationStructureServiceJob.TAG:
                return new LocationStructureServiceJob();
            default:
                Log.w(RevealJobCreator.class.getCanonicalName(), tag + " is not declared in RevealJobCreator Job Creator");
                return null;
        }
    }
}