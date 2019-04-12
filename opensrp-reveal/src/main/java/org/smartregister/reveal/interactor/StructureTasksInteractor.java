package org.smartregister.reveal.interactor;

import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.StructureTasksContract;
import org.smartregister.reveal.util.AppExecutors;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public class StructureTasksInteractor implements StructureTasksContract.Interactor {
    private AppExecutors appExecutors;

    public StructureTasksInteractor() {
        this.appExecutors = RevealApplication.getInstance().getAppExecutors();
    }

    @Override
    public void findTasks(String structureId) {
        appExecutors.diskIO().execute(() -> {
        });
    }
}
