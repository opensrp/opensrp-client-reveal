package org.smartregister.reveal.interactor;

import org.json.JSONObject;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.BaseFormFragmentContract;
import org.smartregister.reveal.util.AppExecutors;

import static org.smartregister.family.util.Utils.metadata;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURE_ID;

/**
 * Created by samuelgithengi on 6/14/19.
 */
public class BaseFormFragmentInteractor implements BaseFormFragmentContract.Interactor {

    private BaseFormFragmentContract.Presenter presenter;

    private CommonRepository commonRepository;

    private AppExecutors appExecutors;

    public BaseFormFragmentInteractor(BaseFormFragmentContract.Presenter presenter) {
        this.presenter = presenter;
        this.commonRepository = RevealApplication.getInstance().getContext().commonrepository(metadata().familyMemberRegister.tableName);
        appExecutors = RevealApplication.getInstance().getAppExecutors();
    }

    @Override
    public void findNumberOfMembers(String structureId, JSONObject formJSON) {
        appExecutors.diskIO().execute(() -> {
            final int numberOfMembers = commonRepository.countSearchIds(
                    String.format("SELECT count(*) FROM %s WHERE %s = '%s'",
                            metadata().familyMemberRegister.tableName, STRUCTURE_ID, structureId));
            appExecutors.mainThread().execute(() -> {
                presenter.onFoundMembersCount(numberOfMembers, formJSON);
            });
        });

    }
}
