package org.smartregister.reveal.interactor;


import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.family.interactor.FamilyOtherMemberProfileInteractor;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.FamilyOtherMemberProfileContract;
import org.smartregister.reveal.contract.FamilyOtherMemberProfileContract.Interactor;
import org.smartregister.reveal.util.AppExecutors;

import static org.smartregister.reveal.application.RevealApplication.getInstance;

public class RevealFamilyOtherMemberInteractor extends FamilyOtherMemberProfileInteractor implements Interactor {

    private FamilyOtherMemberProfileContract.Presenter presenter;

    private CommonRepository commonRepository;

    private AppExecutors appExecutors;

    public RevealFamilyOtherMemberInteractor(FamilyOtherMemberProfileContract.Presenter presenter) {
        this.presenter = presenter;
        commonRepository = getInstance().getContext().commonrepository(getInstance().getMetadata().familyMemberRegister.tableName);
        appExecutors = RevealApplication.getInstance().getAppExecutors();
    }

    @Override
    public void getFamilyHead(String familyHeadId) {
        appExecutors.diskIO().execute(() -> {
            CommonPersonObject commonPersonObject = commonRepository.findByBaseEntityId(familyHeadId);
            appExecutors.mainThread().execute(() -> {
                presenter.onFetchFamilyHead(commonPersonObject);
            });
        });
    }
}
