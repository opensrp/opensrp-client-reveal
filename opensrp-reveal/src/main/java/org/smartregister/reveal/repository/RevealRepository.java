package org.smartregister.reveal.repository;

import android.content.Context;
import android.util.Log;

import android.content.Context;
import android.util.Log;

import net.sqlcipher.database.SQLiteDatabase;

import org.smartregister.AllConstants;
import org.smartregister.configurableviews.repository.ConfigurableViewsRepository;
import org.smartregister.immunization.repository.VaccineRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.repository.Repository;
import org.smartregister.reveal.application.RevealApplication;


public class RevealRepository extends Repository {

    private static final String TAG = RevealRepository.class.getCanonicalName();
    protected SQLiteDatabase readableDatabase;
    protected SQLiteDatabase writableDatabase;

    public RevealRepository(Context context, org.smartregister.Context openSRPContext) {
        super(context, AllConstants.DATABASE_NAME, AllConstants.DATABASE_VERSION, openSRPContext.session(), RevealApplication.createCommonFtsObject(), openSRPContext.sharedRepositoriesArray());
    }

    @Override
    public void onCreate(SQLiteDatabase database) {
        super.onCreate(database);
        ConfigurableViewsRepository.createTable(database);
        EventClientRepository.createTable(database, EventClientRepository.Table.client, EventClientRepository.client_column.values());
        EventClientRepository.createTable(database, EventClientRepository.Table.event, EventClientRepository.event_column.values());
//        UniqueIdRepository.createTable(database);
        VaccineRepository.createTable(database);

        updateVaccineRepository(database);
        //onUpgrade(database, 1, 2);
    }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        Log.w(RevealRepository.class.getName(),
                "Upgrading database from version " + oldVersion + " to "
                        + newVersion + ", which will destroy all old data");

        int upgradeTo = oldVersion + 1;
        while (upgradeTo <= newVersion) {
            switch (upgradeTo) {
                case 2:
                    // upgradeToVersion2(db);
                    break;
                default:
                    break;
            }
            upgradeTo++;
        }
    }

    @Override
    public SQLiteDatabase getReadableDatabase() {
        return getReadableDatabase(RevealApplication.getInstance().getPassword());
    }

    @Override
    public SQLiteDatabase getWritableDatabase() {
        return getWritableDatabase(RevealApplication.getInstance().getPassword());
    }

    @Override
    public synchronized SQLiteDatabase getReadableDatabase(String password) {
        try {
            if (readableDatabase == null || !readableDatabase.isOpen()) {
                if (readableDatabase != null) {
                    readableDatabase.close();
                }
                readableDatabase = super.getReadableDatabase(password);
            }
            return readableDatabase;
        } catch (Exception e) {
            Log.e(TAG, "Database Error. " + e.getMessage());
            return null;
        }

    }

    @Override
    public synchronized SQLiteDatabase getWritableDatabase(String password) {
        if (writableDatabase == null || !writableDatabase.isOpen()) {
            if (writableDatabase != null) {
                writableDatabase.close();
            }
            writableDatabase = super.getWritableDatabase(password);
        }
        return writableDatabase;
    }

    @Override
    public synchronized void close() {
        if (readableDatabase != null) {
            readableDatabase.close();
        }

        if (writableDatabase != null) {
            writableDatabase.close();
        }
        super.close();
    }

    private void updateVaccineRepository(SQLiteDatabase database) {
        database.execSQL(VaccineRepository.UPDATE_TABLE_ADD_EVENT_ID_COL);
        database.execSQL(VaccineRepository.EVENT_ID_INDEX);
        database.execSQL(VaccineRepository.UPDATE_TABLE_ADD_FORMSUBMISSION_ID_COL);
        database.execSQL(VaccineRepository.FORMSUBMISSION_INDEX);
        database.execSQL(VaccineRepository.UPDATE_TABLE_ADD_OUT_OF_AREA_COL);
        database.execSQL(VaccineRepository.UPDATE_TABLE_ADD_OUT_OF_AREA_COL_INDEX);
        database.execSQL(VaccineRepository.UPDATE_TABLE_ADD_HIA2_STATUS_COL);
        database.execSQL(VaccineRepository.ALTER_ADD_CREATED_AT_COLUMN);
        database.execSQL(VaccineRepository.UPDATE_TABLE_ADD_CHILD_LOCATION_ID_COL);
        database.execSQL(VaccineRepository.UPDATE_TABLE_ADD_TEAM_ID_COL);
        database.execSQL(VaccineRepository.UPDATE_TABLE_ADD_TEAM_COL);

        VaccineRepository.migrateCreatedAt(database);
    }
}

