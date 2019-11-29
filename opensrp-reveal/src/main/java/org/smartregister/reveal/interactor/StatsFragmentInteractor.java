package org.smartregister.reveal.interactor;

import net.sqlcipher.database.SQLiteDatabase;
import net.sqlcipher.Cursor;
import net.sqlcipher.SQLException;

import org.smartregister.repository.BaseRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.StatsFragmentContract;
import org.smartregister.reveal.util.AppExecutors;

import java.util.HashMap;
import java.util.Map;

import static org.smartregister.reveal.util.Constants.DatabaseKeys.SYNC_STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.VALIDATION_STATUS;
import static org.smartregister.reveal.util.Constants.SyncInfo.INVALID_CLIENTS;
import static org.smartregister.reveal.util.Constants.SyncInfo.INVALID_EVENTS;
import static org.smartregister.reveal.util.Constants.SyncInfo.NULL_EVENT_SYNC_STATUS;
import static org.smartregister.reveal.util.Constants.SyncInfo.SYNCED_CLIENTS;
import static org.smartregister.reveal.util.Constants.SyncInfo.SYNCED_EVENTS;
import static org.smartregister.reveal.util.Constants.SyncInfo.TASK_UNPROCESSED_EVENTS;
import static org.smartregister.reveal.util.Constants.SyncInfo.UNSYNCED_CLIENTS;
import static org.smartregister.reveal.util.Constants.SyncInfo.UNSYNCED_EVENTS;
import static org.smartregister.reveal.util.Constants.SyncInfo.VALID_CLIENTS;
import static org.smartregister.reveal.util.Constants.SyncInfo.VALID_EVENTS;

public class StatsFragmentInteractor implements StatsFragmentContract.Interactor {

    private AppExecutors appExecutors;

    private SQLiteDatabase database;

    private StatsFragmentContract.Presenter presenter;


    public StatsFragmentInteractor(StatsFragmentContract.Presenter presenter) {
        this.presenter = presenter;
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        database = RevealApplication.getInstance().getRepository().getReadableDatabase();
    }

    @Override
    public void fetchECSyncInfo() {

        Map<String, Integer> syncInfoMap = new HashMap<>();
        syncInfoMap.put(SYNCED_EVENTS, 0);
        syncInfoMap.put(SYNCED_CLIENTS, 0);
        syncInfoMap.put(UNSYNCED_EVENTS, 0);
        syncInfoMap.put(UNSYNCED_CLIENTS, 0);
        syncInfoMap.put(VALID_EVENTS, 0);
        syncInfoMap.put(INVALID_EVENTS, 0);
        syncInfoMap.put(VALID_CLIENTS, 0);
        syncInfoMap.put(INVALID_CLIENTS, 0);
        syncInfoMap.put(TASK_UNPROCESSED_EVENTS, 0);
        syncInfoMap.put(NULL_EVENT_SYNC_STATUS, 0);

        String eventSyncSql = "select count(*), syncStatus from event group by syncStatus";
        String clientSyncSql = "select count(*), syncStatus from client group by syncStatus";

        String validatedEventsSql = "select count(*), validationStatus from event group by validationStatus";
        String validatedClientsSql = "select count(*), validationStatus from client group by validationStatus";


        Cursor cursor;

        try {
            cursor = database.rawQuery(eventSyncSql, new String[] {});
            while (cursor.moveToNext()) {
                String syncStatus = cursor.getString(cursor.getColumnIndex(SYNC_STATUS));
                if (BaseRepository.TYPE_Synced.equals(syncStatus)) {
                    syncInfoMap.put(SYNCED_EVENTS, cursor.getInt(0));
                } else if (BaseRepository.TYPE_Unsynced.equals(syncStatus)) {
                    syncInfoMap.put(UNSYNCED_EVENTS, cursor.getInt(0));
                } else if (BaseRepository.TYPE_Task_Unprocessed.equals(syncStatus)) {
                    syncInfoMap.put(TASK_UNPROCESSED_EVENTS, cursor.getInt(0));
                } else if (syncStatus == null) {
                    syncInfoMap.put(NULL_EVENT_SYNC_STATUS, cursor.getInt(0));
                }
            }
            cursor.close();


            cursor = database.rawQuery(clientSyncSql, new String[] {});
            while (cursor.moveToNext()) {
                String syncStatus = cursor.getString(cursor.getColumnIndex(SYNC_STATUS));
                if (BaseRepository.TYPE_Synced.equals(syncStatus)) {
                    syncInfoMap.put(SYNCED_CLIENTS, cursor.getInt(0));
                } else if (BaseRepository.TYPE_Unsynced.equals(syncStatus)) {
                    syncInfoMap.put(UNSYNCED_CLIENTS, cursor.getInt(0));
                }
            }
            cursor.close();


            cursor = database.rawQuery(validatedEventsSql, new String[] {});
            while (cursor.moveToNext()) {
                String syncStatus = cursor.getString(cursor.getColumnIndex(VALIDATION_STATUS));
                if (BaseRepository.TYPE_Valid.equals(syncStatus)) {
                    syncInfoMap.put(VALID_EVENTS, cursor.getInt(0));
                } else if (BaseRepository.TYPE_InValid.equals(syncStatus)) {
                    syncInfoMap.put(INVALID_EVENTS, cursor.getInt(0));
                }
            }
            cursor.close();

            cursor = database.rawQuery(validatedClientsSql, new String[] {});
            while (cursor.moveToNext()) {
                String validationStatus = cursor.getString(cursor.getColumnIndex(VALIDATION_STATUS));
                if (BaseRepository.TYPE_Valid.equals(validationStatus)) {
                    syncInfoMap.put(VALID_CLIENTS, cursor.getInt(0));
                } else if (BaseRepository.TYPE_InValid.equals(validationStatus)) {
                    syncInfoMap.put(INVALID_CLIENTS, cursor.getInt(0));
                }
            }

            cursor.close();

            appExecutors.mainThread().execute(new Runnable() {
                @Override
                public void run() {
                    presenter.onECSyncInfoFetched(syncInfoMap);
                }
            });


        } catch (Exception e) {
            e.printStackTrace();
        }


    }
}
