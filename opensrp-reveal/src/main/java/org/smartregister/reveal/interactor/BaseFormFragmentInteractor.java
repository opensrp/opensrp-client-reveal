package org.smartregister.reveal.interactor;

import net.sqlcipher.Cursor;
import net.sqlcipher.database.SQLiteDatabase;

import org.json.JSONArray;
import org.json.JSONObject;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.BaseFormFragmentContract;
import org.smartregister.reveal.util.AppExecutors;

import timber.log.Timber;

import static com.vijay.jsonwizard.constants.JsonFormConstants.KEY;
import static com.vijay.jsonwizard.constants.JsonFormConstants.TEXT;
import static org.smartregister.family.util.Utils.metadata;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.BASE_ENTITY_ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.FIRST_NAME;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.LAST_NAME;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURE_ID;

/**
 * Created by samuelgithengi on 6/14/19.
 */
public class BaseFormFragmentInteractor implements BaseFormFragmentContract.Interactor {

    private BaseFormFragmentContract.Presenter presenter;

    private CommonRepository commonRepository;

    private AppExecutors appExecutors;

    private SQLiteDatabase sqLiteDatabase;

    public BaseFormFragmentInteractor(BaseFormFragmentContract.Presenter presenter) {
        this.presenter = presenter;
        this.commonRepository = RevealApplication.getInstance().getContext().commonrepository(metadata().familyMemberRegister.tableName);
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        sqLiteDatabase = RevealApplication.getInstance().getRepository().getReadableDatabase();
    }

    @Override
    public void findNumberOfMembers(String structureId, JSONObject formJSON) {
        appExecutors.diskIO().execute(() -> {
            final int numberOfMembers = commonRepository.countSearchIds(
                    String.format("SELECT count(*) FROM %s WHERE %s = '%s'",
                            metadata().familyMemberRegister.tableName, STRUCTURE_ID, structureId));
            appExecutors.mainThread().execute(() -> {
                presenter.onFetchedMembersCount(numberOfMembers, formJSON);
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


}
