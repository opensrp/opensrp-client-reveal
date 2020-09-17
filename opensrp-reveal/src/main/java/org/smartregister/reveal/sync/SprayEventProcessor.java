package org.smartregister.reveal.sync;

import android.content.ContentValues;
import android.database.Cursor;

import net.sqlcipher.database.SQLiteDatabase;

import org.smartregister.domain.Client;
import org.smartregister.domain.Event;
import org.smartregister.domain.Obs;
import org.smartregister.domain.jsonmapping.ClientClassification;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.Constants.DatabaseKeys;
import org.smartregister.reveal.util.Constants.Tables;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

/**
 * Created by samuelgithengi on 9/17/20.
 */
public class SprayEventProcessor {

    public void processSprayEvent(RevealClientProcessor clientProcessor, ClientClassification clientClassification, Event event, boolean isLocalEvent) throws Exception {


        ClientClassification normalClassification = new ClientClassification();
        normalClassification.case_classification_rules = Collections.singletonList(clientClassification.case_classification_rules.get(0));
        ClientClassification ecEventsClassification = new ClientClassification();
        ecEventsClassification.case_classification_rules = Collections.singletonList(clientClassification.case_classification_rules.get(1));


        Client client = new Client(event.getBaseEntityId());
        clientProcessor.processEvent(event, client, normalClassification);

        if (event.getDetails() == null)
            event.setDetails(new HashMap<>());
        event.getDetails().put(DatabaseKeys.FORM_SUBMISSION_ID, event.getFormSubmissionId());

        if (isLocalEvent) {
            Obs mopup = event.findObs(null, true, "mopup");
            if (mopup != null) {
                event.setFormSubmissionId(event.getBaseEntityId() + ":" + mopup.getValue());
            } else {
                event.setFormSubmissionId(event.getBaseEntityId());
            }
        } else {
            SQLiteDatabase sqLiteDatabase = RevealApplication.getInstance().getRepository().getWritableDatabase();
            List<String> formSubmissions = getFormSubmissions(event.getBaseEntityId(), event.getEventType(), sqLiteDatabase);
            if (formSubmissions.size() == 2) {
                sqLiteDatabase.beginTransaction();
                sqLiteDatabase.delete(Tables.EC_EVENTS_TABLE, String.format("%s=?", DatabaseKeys.FORM_SUBMISSION_ID), new String[]{formSubmissions.get(0)});
                ContentValues contentValues = new ContentValues();
                contentValues.put(DatabaseKeys.BASE_ENTITY_ID, event.getBaseEntityId());
                sqLiteDatabase.update(Tables.EC_EVENTS_TABLE, contentValues, String.format("%s=?", DatabaseKeys.FORM_SUBMISSION_ID), new String[]{formSubmissions.get(1)});
                sqLiteDatabase.setTransactionSuccessful();
                sqLiteDatabase.endTransaction();
            }
        }
        clientProcessor.processEvent(event, client, ecEventsClassification);
    }


    public List<String> getFormSubmissions(String baseEntityId, String eventType, SQLiteDatabase sqLiteDatabase) {
        List<String> formSubmissions = new ArrayList<>();
        try (Cursor cursor = sqLiteDatabase.rawQuery(
                String.format("SELECT %s from %s WHERE %s IN (?,?) AND %s=? ORDER BY %s",
                        DatabaseKeys.FORM_SUBMISSION_ID, Tables.EC_EVENTS_TABLE, DatabaseKeys.BASE_ENTITY_ID, DatabaseKeys.EVENT_TYPE, DatabaseKeys.VERSION),
                new String[]{baseEntityId + ":yes", baseEntityId + ":no", eventType})) {
            while (cursor.moveToNext()) {
                formSubmissions.add(cursor.getString(0));
            }
        }

        return formSubmissions;

    }
}
