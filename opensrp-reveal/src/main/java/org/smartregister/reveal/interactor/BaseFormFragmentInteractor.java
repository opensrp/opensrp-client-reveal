package org.smartregister.reveal.interactor;

import androidx.core.util.Pair;

import net.sqlcipher.Cursor;
import net.sqlcipher.database.SQLiteDatabase;

import org.json.JSONArray;
import org.json.JSONObject;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.BaseFormFragmentContract;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.InteractorUtils;

import timber.log.Timber;

import static com.vijay.jsonwizard.constants.JsonFormConstants.KEY;
import static com.vijay.jsonwizard.constants.JsonFormConstants.TEXT;
import static org.smartregister.family.util.Utils.metadata;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.BASE_ENTITY_ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.FIRST_NAME;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.LAST_NAME;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURE_ID;
import static org.smartregister.reveal.util.Constants.Intervention.IRS;

/**
 * Created by samuelgithengi on 6/14/19.
 */
public class BaseFormFragmentInteractor implements BaseFormFragmentContract.Interactor {

    private BaseFormFragmentContract.Presenter presenter;

    private CommonRepository commonRepository;

    private AppExecutors appExecutors;

    private SQLiteDatabase sqLiteDatabase;

    protected EventClientRepository eventClientRepository;

    private InteractorUtils interactorUtils;

    public BaseFormFragmentInteractor(BaseFormFragmentContract.Presenter presenter) {
        this.presenter = presenter;
        this.commonRepository = RevealApplication.getInstance().getContext().commonrepository(metadata().familyMemberRegister.tableName);
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        sqLiteDatabase = RevealApplication.getInstance().getRepository().getReadableDatabase();
        eventClientRepository = RevealApplication.getInstance().getContext().getEventClientRepository();
        interactorUtils = new InteractorUtils();
    }

    @Override
    public void findNumberOfMembers(String structureId, JSONObject formJSON) {
        appExecutors.diskIO().execute(() -> {
            Cursor cursor = null;
            int numberOfMembers = 0;
            int numberOfMembersSleepingOutdoors = 0;
            try {
                cursor = sqLiteDatabase.rawQuery(
                        String.format("SELECT count(*),SUM(CASE WHEN sleeps_outdoors='Yes' THEN 1 ELSE 0 END) FROM %s WHERE %s = ?",
                                metadata().familyMemberRegister.tableName, STRUCTURE_ID), new String[]{structureId});

                while (cursor.moveToNext()) {
                    numberOfMembers = cursor.getInt(0);
                    numberOfMembersSleepingOutdoors = cursor.getInt(1);
                }
            } catch (Exception e) {
                Timber.e(e, "Error find Number of members ");
            } finally {
                if (cursor != null)
                    cursor.close();
            }
            int finalNumberOfMembers = numberOfMembers;
            int finalNumberOfMembersSleepingOutdoors = numberOfMembersSleepingOutdoors;
            appExecutors.mainThread().execute(() -> {
                presenter.onFetchedMembersCount(new Pair<>(finalNumberOfMembers, finalNumberOfMembersSleepingOutdoors), formJSON);
            });
        });

    }

    @Override
    public void findMemberDetails(String structureId, JSONObject formJSON) {
        appExecutors.diskIO().execute(() -> {
            JSONArray familyMembers = new JSONArray();
            Cursor cursor = null;
            try {
                cursor = sqLiteDatabase.rawQuery(
                        String.format("SELECT %s, %s, %s FROM %s WHERE %s = ?", BASE_ENTITY_ID, FIRST_NAME, LAST_NAME,
                                metadata().familyMemberRegister.tableName, STRUCTURE_ID), new String[]{structureId});
                while (cursor.moveToNext()) {
                    JSONObject member = new JSONObject();
                    member.put(KEY, cursor.getString(cursor.getColumnIndex(BASE_ENTITY_ID)));
                    member.put(TEXT, String.format("%s %s", cursor.getString(cursor.getColumnIndex(FIRST_NAME))
                            , cursor.getString(cursor.getColumnIndex(LAST_NAME))));
                    familyMembers.put(member);
                }
            } catch (Exception e) {
                Timber.e(e, "Error find Member Details ");
            } finally {
                if (cursor != null)
                    cursor.close();
            }
            appExecutors.mainThread().execute(() -> {
                presenter.onFetchedFamilyMembers(familyMembers, formJSON);
            });
        });
    }

    @Override
    public void findSprayDetails(String interventionType, String structureId, JSONObject formJSON) {
        if (IRS.equals(interventionType)) {

            appExecutors.diskIO().execute(() -> {
                CommonPersonObject commonPersonObject = interactorUtils.fetchSprayDetails(interventionType, structureId,
                        eventClientRepository, commonRepository);

                appExecutors.mainThread().execute(() -> {
                    presenter.onFetchedSprayDetails(commonPersonObject, formJSON);
                });
            });
        }
    }


}
