package org.smartregister.reveal.sync;

import androidx.annotation.VisibleForTesting;

import net.sqlcipher.Cursor;
import net.sqlcipher.database.SQLiteDatabase;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.domain.Client;
import org.smartregister.domain.Event;
import org.smartregister.domain.Obs;
import org.smartregister.domain.jsonmapping.ClientClassification;
import org.smartregister.repository.EventClientRepository.event_column;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.util.Constants.DatabaseKeys;
import org.smartregister.reveal.util.Constants.Tables;

import java.util.Collections;
import java.util.HashMap;

import static org.smartregister.commonregistry.CommonRepository.ID_COLUMN;
import static org.smartregister.family.util.DBConstants.KEY.OBJECT_ID;
import static org.smartregister.reveal.util.Constants.JsonForm.BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.JsonForm.MOP_UP;

/**
 * Created by samuelgithengi on 9/17/20.
 */
public class SprayEventProcessor {

    private static final String YES = "yes";

    private SQLiteDatabase sqLiteDatabase;

    @VisibleForTesting
    public SprayEventProcessor(SQLiteDatabase sqLiteDatabase) {
        this.sqLiteDatabase = sqLiteDatabase;
    }

    public SprayEventProcessor() {
    }

    public void processSprayEvent(RevealClientProcessor clientProcessor, ClientClassification clientClassification, Event event, boolean isLocalEvent) throws Exception {


        ClientClassification normalClassification = new ClientClassification();
        normalClassification.case_classification_rules = Collections.singletonList(clientClassification.case_classification_rules.get(0));
        ClientClassification ecEventsClassification = new ClientClassification();
        ecEventsClassification.case_classification_rules = Collections.singletonList(clientClassification.case_classification_rules.get(1));


        Client client = new Client(event.getBaseEntityId());
        clientProcessor.processEvent(event, client, normalClassification);

        String formSubmissionId = event.getFormSubmissionId();
        if (event.getDetails() == null)
            event.setDetails(new HashMap<>());
        event.getDetails().put(DatabaseKeys.FORM_SUBMISSION_ID, formSubmissionId);

        Obs mopUp = event.findObs(null, true, MOP_UP);

        if (mopUp != null && YES.equalsIgnoreCase(mopUp.getValue().toString())) {
            try (Cursor cursor = getSqLiteDatabase().query(Tables.EVENT_TABLE, new String[]{event_column.json.name()}
                    , String.format("%s like ? AND %s=?", event_column.baseEntityId, event_column.eventType.name()),
                    new String[]{event.getBaseEntityId() + "%", event.getEventType()}, null, null, event_column.updatedAt.name() + " DESC", "1")) {
                if (cursor.moveToNext()) {
                    Event previousEvent = RevealApplication.getInstance().getContext().getEventClientRepository().convert(cursor.getString(0), Event.class);
                    Obs previousBusinessStatus = previousEvent.findObs(null, true, BUSINESS_STATUS);
                    Obs businessStatus = event.findObs(null, true, BUSINESS_STATUS);
                    if (businessStatus != null && businessStatus.getValue().toString().equalsIgnoreCase(previousBusinessStatus.getValue().toString())) {
                        event.setFormSubmissionId(event.getBaseEntityId() + ":" + businessStatus.getValue().toString());
                    } else {
                        event.setFormSubmissionId(event.getBaseEntityId());
                    }
                }
            }

        } else {
            event.setFormSubmissionId(event.getBaseEntityId());
        }
        if (!isLocalEvent && StringUtils.isNotBlank(event.getBaseEntityId())) {
            getSqLiteDatabase().delete(Tables.EC_EVENTS_TABLE,
                    String.format("%s like ? AND %s=?", ID_COLUMN, DatabaseKeys.EVENT_TYPE),
                    new String[]{event.getBaseEntityId() + "%", event.getEventType()});

            getSqLiteDatabase().delete(Tables.EC_EVENTS_SEARCH_TABLE,
                    String.format("%s like ? AND %s=?", OBJECT_ID, DatabaseKeys.EVENT_TYPE),
                    new String[]{event.getBaseEntityId() + "%", event.getEventType()});

        }
        clientProcessor.processEvent(event, client, ecEventsClassification);
        event.setFormSubmissionId(formSubmissionId);
    }


    private SQLiteDatabase getSqLiteDatabase() {
        if (sqLiteDatabase == null) {
            sqLiteDatabase = RevealApplication.getInstance().getRepository().getWritableDatabase();
        }
        return sqLiteDatabase;
    }

}
