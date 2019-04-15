package org.smartregister.reveal.presenter;

import android.util.Log;

import net.sqlcipher.Cursor;
import net.sqlcipher.database.SQLiteDatabase;

import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.family.presenter.BaseFamilyProfilePresenter;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.FamilyProfileContract;
import org.smartregister.reveal.interactor.RevealFamilyProfileInteractor;
import org.smartregister.reveal.model.FamilyProfileModel;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.TaskUtils;
import org.smartregister.reveal.util.Utils;

import static org.smartregister.family.util.Constants.INTENT_KEY.BASE_ENTITY_ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURE_ID;
import static org.smartregister.reveal.util.FamilyConstants.TABLE_NAME.FAMILY;

/**
 * Created by samuelgithengi on 4/10/19.
 */
public class FamilyProfilePresenter extends BaseFamilyProfilePresenter {
    private AppExecutors appExecutors;
    private SQLiteDatabase database;
    private String structureId;
    private PreferencesUtil preferencesUtil;
    private TaskUtils taskUtils;

    public FamilyProfilePresenter(FamilyProfileContract.View view, FamilyProfileContract.Model model, String familyBaseEntityId, String familyHead, String primaryCaregiver, String familyName) {
        super(view, model, familyBaseEntityId, familyHead, primaryCaregiver, familyName);
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        database = RevealApplication.getInstance().getRepository().getReadableDatabase();
        preferencesUtil = PreferencesUtil.getInstance();
        taskUtils = TaskUtils.getInstance();
        getStructureId(familyBaseEntityId);
        setInteractor(new RevealFamilyProfileInteractor());
    }

    @Override
    public void refreshProfileTopSection(CommonPersonObjectClient client) {
        super.refreshProfileTopSection(client);
        getView().setProfileDetailOne(preferencesUtil.getCurrentOperationalArea());
        getView().setProfileDetailTwo(preferencesUtil.getCurrentDistrict());
    }


    private void getStructureId(String familyId) {
        appExecutors.diskIO().execute(() -> {
            Cursor cursor = null;
            try {
                cursor = database.rawQuery(String.format("SELECT DISTINCT %s FROM %S WHERE %s = ?",
                        STRUCTURE_ID, FAMILY, BASE_ENTITY_ID), new String[]{familyId});
                if (cursor.moveToNext()) {
                    structureId = cursor.getString(0);
                }
            } catch (Exception e) {
                Log.e(TAG, "Error getting residence for" + familyId, e);
            } finally {
                if (cursor != null)
                    cursor.close();
            }
            if (structureId != null) {
                appExecutors.mainThread().execute(() -> {
                    getModel().setStructureId(structureId);
                    getView().setStructureId(structureId);
                });
            }
        });
    }

    private FamilyProfileModel getModel() {
        return (FamilyProfileModel) model;
    }

    public String getStructureId() {
        return structureId;
    }

    @Override
    public FamilyProfileContract.View getView() {
        return (FamilyProfileContract.View) super.getView();
    }

    @Override
    public void onRegistrationSaved(boolean isEdit) {
        super.onRegistrationSaved(isEdit);
        if (!isEdit && Utils.getInterventionLabel() == R.string.focus_investigation) {
            taskUtils.generateBloodScreeningTask(getView().getApplicationContext(),
                    getModel().getEventClient().getEvent().getBaseEntityId());
        }

    }
}
