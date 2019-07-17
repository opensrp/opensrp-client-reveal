package org.smartregister.reveal.interactor;

import android.content.Context;

import org.smartregister.domain.db.EventClient;
import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.FamilyRegisterContract;
import org.smartregister.reveal.sync.RevealClientProcessor;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.TaskUtils;
import org.smartregister.sync.ClientProcessorForJava;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * Created by samuelgithengi on 4/15/19.
 */
public class RevealFamilyRegisterInteractor extends org.smartregister.family.interactor.FamilyRegisterInteractor implements FamilyRegisterContract.Interactor {

    private TaskUtils taskUtils;

    private AppExecutors appExecutors;

    private RevealClientProcessor clientProcessor;

    private FamilyRegisterContract.Presenter presenter;

    public RevealFamilyRegisterInteractor(FamilyRegisterContract.Presenter presenter) {
        this.presenter = presenter;
        taskUtils = TaskUtils.getInstance();
        clientProcessor = (RevealClientProcessor) RevealApplication.getInstance().getClientProcessor();
        appExecutors = RevealApplication.getInstance().getAppExecutors();
    }

    @Override
    public ClientProcessorForJava getClientProcessorForJava() {
        return RevealClientProcessor.getInstance(RevealApplication.getInstance().getApplicationContext());
    }

    @Override
    public void generateTasks(List<FamilyEventClient> eventClientList, String structureId, Context context) {
        appExecutors.diskIO().execute(() -> {
            Set<String> generatedIds = new HashSet<>();
            for (FamilyEventClient eventClient : eventClientList) {
                if (eventClient.getClient().getLastName().equals("Family"))
                    continue;
                String entityId = eventClient.getClient().getBaseEntityId();
                if (!generatedIds.contains(entityId)) {
                    generatedIds.add(entityId);
                    taskUtils.generateBloodScreeningTask(context, entityId,structureId);
                }
            }
            taskUtils.generateBedNetDistributionTask(context, structureId);
            appExecutors.mainThread().execute(() -> presenter.onTasksGenerated());
        });
    }

    @Override
    protected void processClient(List<EventClient> eventClientList) {
        clientProcessor.processClient(eventClientList, true);
    }
}
