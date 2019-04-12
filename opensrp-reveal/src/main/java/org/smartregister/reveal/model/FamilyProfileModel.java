package org.smartregister.reveal.model;

import net.sqlcipher.Cursor;
import net.sqlcipher.database.SQLiteDatabase;

import org.smartregister.family.domain.FamilyEventClient;
import org.smartregister.family.model.BaseFamilyProfileModel;
import org.smartregister.family.util.DBConstants;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Constants.DatabaseKeys;
import org.smartregister.reveal.util.FamilyConstants;
import org.smartregister.reveal.util.FamilyConstants.TABLE_NAME;

import static org.smartregister.family.util.Constants.INTENT_KEY.BASE_ENTITY_ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURE_ID;
import static org.smartregister.reveal.util.FamilyConstants.RELATIONSHIP.RESIDENCE;
import static org.smartregister.reveal.util.FamilyConstants.TABLE_NAME.*;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public class FamilyProfileModel extends BaseFamilyProfileModel {

    private final String familyId;
    private String structureId;

    private AppExecutors appExecutors;

    private SQLiteDatabase database;

    public FamilyProfileModel(String familyName, String familyId) {
        super(familyName);
        this.familyId = familyId;
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        database = RevealApplication.getInstance().getRepository().getReadableDatabase();
        getStructureId();
    }

    private void getStructureId() {
        appExecutors.diskIO().execute(() -> {
            Cursor cursor = null;
            try {
                cursor = database.rawQuery(String.format("SELECT DISTINCT %s FROM %S WHERE %s = ?",
                        STRUCTURE_ID, FAMILY, BASE_ENTITY_ID), new String[]{familyId});
                if (cursor.moveToNext()) {
                    structureId = cursor.getString(0);
                }
            } finally {
                if (cursor != null)
                    cursor.close();
            }
        });
    }

    @Override
    public FamilyEventClient processMemberRegistration(String jsonString, String familyBaseEntityId) {
        FamilyEventClient eventClient = super.processMemberRegistration(jsonString, familyBaseEntityId);
        if (structureId != null)
            eventClient.getClient().addAttribute(RESIDENCE, structureId);
        return eventClient;
    }

    @Override
    public FamilyEventClient processFamilyRegistrationForm(String jsonString, String familyBaseEntityId) {
        FamilyEventClient eventClient = super.processFamilyRegistrationForm(jsonString, familyBaseEntityId);
        if (structureId != null)
            eventClient.getClient().addAttribute(RESIDENCE, structureId);
        return eventClient;
    }

    @Override
    public FamilyEventClient processUpdateMemberRegistration(String jsonString, String familyBaseEntityId) {
        FamilyEventClient eventClient = super.processUpdateMemberRegistration(jsonString, familyBaseEntityId);
        if (structureId != null)
            eventClient.getClient().addAttribute(RESIDENCE, structureId);
        return eventClient;
    }
}
