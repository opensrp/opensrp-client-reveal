package org.smartregister.reveal.interactor;

import android.content.Context;

import org.smartregister.family.interactor.FamilyProfileInteractor;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.FamilyProfileContract;
import org.smartregister.reveal.sync.RevealClientProcessor;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.TaskUtils;
import org.smartregister.sync.ClientProcessorForJava;

/**
 * Created by samuelgithengi on 4/15/19.
 */
public class RevealFamilyProfileInteractor extends FamilyProfileInteractor implements FamilyProfileContract.Interactor {

    private TaskUtils taskUtils;

    private AppExecutors appExecutors;
    private FamilyProfileContract.Presenter presenter;

    public RevealFamilyProfileInteractor(FamilyProfileContract.Presenter presenter) {
        this.presenter = presenter;
        taskUtils = TaskUtils.getInstance();
        appExecutors = RevealApplication.getInstance().getAppExecutors();
    }

    @Override
    public ClientProcessorForJava getClientProcessorForJava() {
        return RevealClientProcessor.getInstance(RevealApplication.getInstance().getApplicationContext());
    }

    @Override
    public void generateTasks(Context applicationContext, String baseEntityId) {
        appExecutors.diskIO().execute(() -> {
            taskUtils.generateBloodScreeningTask(applicationContext,
                    baseEntityId);
            appExecutors.mainThread().execute(() -> {
                presenter.onTasksGenerated();
            });
        });
    }
}
