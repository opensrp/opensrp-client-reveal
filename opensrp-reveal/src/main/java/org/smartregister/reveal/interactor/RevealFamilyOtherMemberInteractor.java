package org.smartregister.reveal.interactor;


import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.domain.Task;
import org.smartregister.family.interactor.FamilyOtherMemberProfileInteractor;
import org.smartregister.repository.TaskRepository;
import org.smartregister.reveal.contract.FamilyOtherMemberProfileContract;
import org.smartregister.reveal.contract.FamilyOtherMemberProfileContract.Interactor;
import org.smartregister.reveal.util.AppExecutors;

import static org.smartregister.reveal.application.RevealApplication.getInstance;

public class RevealFamilyOtherMemberInteractor extends FamilyOtherMemberProfileInteractor implements Interactor {

    private CommonRepository commonRepository;

    private AppExecutors appExecutors;

    private TaskRepository taskRepository;

    public RevealFamilyOtherMemberInteractor() {
        commonRepository = getInstance().getContext().commonrepository(getInstance().getMetadata().familyMemberRegister.tableName);
        appExecutors = getInstance().getAppExecutors();
        taskRepository = getInstance().getTaskRepository();
    }

    @Override
    public void getFamilyHead(FamilyOtherMemberProfileContract.BasePresenter presenter, String familyHeadId) {
        appExecutors.diskIO().execute(() -> {
            CommonPersonObject commonPersonObject = commonRepository.findByBaseEntityId(familyHeadId);
            appExecutors.mainThread().execute(() -> {
                presenter.onFetchFamilyHead(commonPersonObject);
            });
        });
    }

    @Override
    public void archiveFamilyMember(CommonPersonObjectClient client) {

        appExecutors.diskIO().execute(() -> {
            taskRepository.cancelTasksByEntityAndStatus(client.entityId(), Task.TaskStatus.READY);
        });
    }
}
