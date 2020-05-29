package org.smartregister.reveal.repository;

import android.content.Context;

import net.sqlcipher.database.SQLiteDatabase;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.AllConstants;
import org.smartregister.configurableviews.repository.ConfigurableViewsRepository;
import org.smartregister.domain.db.EventClient;
import org.smartregister.job.PullUniqueIdsServiceJob;
import org.smartregister.repository.CampaignRepository;
import org.smartregister.repository.ClientFormRepository;
import org.smartregister.repository.EventClientRepository;
import org.smartregister.repository.LocationRepository;
import org.smartregister.repository.ManifestRepository;
import org.smartregister.repository.PlanDefinitionRepository;
import org.smartregister.repository.PlanDefinitionSearchRepository;
import org.smartregister.repository.Repository;
import org.smartregister.repository.SettingsRepository;
import org.smartregister.repository.StructureRepository;
import org.smartregister.repository.TaskRepository;
import org.smartregister.repository.UniqueIdRepository;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.sync.RevealClientProcessor;
import org.smartregister.reveal.util.Constants.DatabaseKeys;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.util.FamilyConstants.EventType;
import org.smartregister.reveal.util.Utils;
import org.smartregister.util.DatabaseMigrationUtils;
import org.smartregister.util.RecreateECUtil;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

import timber.log.Timber;

import static org.smartregister.repository.EventClientRepository.Table.event;
import static org.smartregister.repository.EventClientRepository.event_column.baseEntityId;
import static org.smartregister.repository.EventClientRepository.event_column.eventType;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.BASE_ENTITY_ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.EVENT_TASK_TABLE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.PROPERTY_TYPE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.SPRAYED_STRUCTURES;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.SPRAY_STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURE_ID;
import static org.smartregister.reveal.util.Constants.EventType.PAOT_EVENT;
import static org.smartregister.reveal.util.Constants.LARVAL_DIPPING_EVENT;
import static org.smartregister.reveal.util.Constants.MOSQUITO_COLLECTION_EVENT;
import static org.smartregister.reveal.util.Constants.SPRAY_EVENT;
import static org.smartregister.reveal.util.Constants.STRUCTURE;
import static org.smartregister.reveal.util.Constants.StructureType.RESIDENTIAL;
import static org.smartregister.reveal.util.Constants.Tables.LARVAL_DIPPINGS_TABLE;
import static org.smartregister.reveal.util.Constants.Tables.MOSQUITO_COLLECTIONS_TABLE;
import static org.smartregister.reveal.util.Constants.Tables.PAOT_TABLE;
import static org.smartregister.reveal.util.FamilyConstants.TABLE_NAME.FAMILY;
import static org.smartregister.reveal.util.FamilyConstants.TABLE_NAME.FAMILY_MEMBER;
import static org.smartregister.util.DatabaseMigrationUtils.isColumnExists;


public class RevealRepository extends Repository {

    protected SQLiteDatabase readableDatabase;
    protected SQLiteDatabase writableDatabase;


    public RevealRepository(Context context, org.smartregister.Context openSRPContext) {
        super(context, AllConstants.DATABASE_NAME, BuildConfig.DATABASE_VERSION, openSRPContext.session(), RevealApplication.createCommonFtsObject(), openSRPContext.sharedRepositoriesArray());
    }

    @Override
    public void onCreate(SQLiteDatabase database) {
        super.onCreate(database);
        ConfigurableViewsRepository.createTable(database);
        EventClientRepository.createTable(database, EventClientRepository.Table.client, EventClientRepository.client_column.values());
        EventClientRepository.createTable(database, event, EventClientRepository.event_column.values());

        CampaignRepository.createTable(database);
        TaskRepository.createTable(database);
        LocationRepository.createTable(database);
        StructureRepository.createTable(database);

        onUpgrade(database, 1, BuildConfig.DATABASE_VERSION);
    }

    @Override
    public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
        Timber.w("Upgrading database from version " + oldVersion + " to "
                + newVersion + ", which will destroy all old data");

        int upgradeTo = oldVersion + 1;
        while (upgradeTo <= newVersion) {
            switch (upgradeTo) {
                case 2:
                    upgradeToVersion2(db);
                    break;
                case 3:
                    upgradeToVersion3(db);
                    break;
                case 4:
                    upgradeToVersion4(db);
                    break;
                case 5:
                    upgradeToVersion5(db);
                    break;
                case 6:
                    upgradeToVersion6(db);
                    break;
                default:
                    break;
            }
            upgradeTo++;
        }
    }

    private void upgradeToVersion2(SQLiteDatabase db) {
        SettingsRepository.onUpgrade(db);

        UniqueIdRepository.createTable(db);

        if (!isColumnExists(db, SPRAYED_STRUCTURES, DatabaseKeys.STRUCTURE_NAME)) {
            db.execSQL(String.format("ALTER TABLE %s ADD COLUMN %s VARCHAR ", SPRAYED_STRUCTURES, DatabaseKeys.STRUCTURE_NAME));
        }

        DatabaseMigrationUtils.createAddedECTables(db,
                new HashSet<>(Arrays.asList(FAMILY, FAMILY_MEMBER)),
                RevealApplication.createCommonFtsObject());

        //client process family events after 5 seconds so that get calls to getDatabase return
        new Timer().schedule(new TimerTask() {
            @Override
            public void run() {
                EventClientRepository ecRepository = RevealApplication.getInstance().getContext().getEventClientRepository();
                List<EventClient> eventClientList = ecRepository.fetchEventClientsByEventTypes(
                        Arrays.asList(EventType.FAMILY_REGISTRATION, EventType.FAMILY_MEMBER_REGISTRATION,
                                EventType.UPDATE_FAMILY_REGISTRATION, EventType.UPDATE_FAMILY_MEMBER_REGISTRATION));
                RevealClientProcessor.getInstance(RevealApplication.getInstance().getApplicationContext()).processClient(eventClientList);
            }
        }, 5000);

        PullUniqueIdsServiceJob.scheduleJobImmediately(PullUniqueIdsServiceJob.TAG);

        PlanDefinitionRepository.createTable(db);
        PlanDefinitionSearchRepository.createTable(db);
    }

    private void upgradeToVersion3(SQLiteDatabase db) {
        String createFamilyMemberIndex = String.format("CREATE INDEX family_member_index ON %s (\n" +
                "    %s COLLATE NOCASE, %s COLLATE NOCASE\n" +
                ");", FAMILY_MEMBER, STRUCTURE_ID, BASE_ENTITY_ID);

        String createFamilyResidenceIndex = String.format("CREATE INDEX family_residence_index ON %s (\n" +
                "    %s COLLATE NOCASE \n" +
                ");", FAMILY, STRUCTURE_ID);

        db.execSQL(createFamilyMemberIndex);
        db.execSQL(createFamilyResidenceIndex);
    }

    private void upgradeToVersion4(SQLiteDatabase db) {
        RecreateECUtil util = new RecreateECUtil();
        //recreate spray events
        String query = String.format("select * from %s where %s=? and %s is not null and %s not in (select %s from %s where %s=?) ", SPRAYED_STRUCTURES, PROPERTY_TYPE, SPRAY_STATUS, BASE_ENTITY_ID, baseEntityId, event.name(), eventType);
        String[] params = new String[]{RESIDENTIAL, SPRAY_EVENT};
        Utils.recreateEventAndClients(query, params, db, Utils.getFormTag(), SPRAYED_STRUCTURES, SPRAY_EVENT, STRUCTURE, util);


        //recreate family events and clients
        query = String.format("select * from %s where %s not in (select %s from %s ) ", FAMILY, BASE_ENTITY_ID, baseEntityId, event.name());
        Utils.recreateEventAndClients(query, new String[]{}, db, Utils.getFormTag(), FAMILY, EventType.FAMILY_REGISTRATION, FAMILY, util);


        //recreate family member events and clients
        query = String.format("select * from %s where %s not in (select %s from %s ) ", FAMILY_MEMBER, BASE_ENTITY_ID, baseEntityId, event.name());
        Utils.recreateEventAndClients(query, new String[]{}, db, Utils.getFormTag(), FAMILY_MEMBER, EventType.FAMILY_MEMBER_REGISTRATION, FAMILY_MEMBER, util);


        //recreate larval dipping events and clients
        query = String.format("select * from %s where %s not in (select %s from %s ) ", LARVAL_DIPPINGS_TABLE, BASE_ENTITY_ID, baseEntityId, event.name());
        Utils.recreateEventAndClients(query, new String[]{}, db, Utils.getFormTag(), LARVAL_DIPPINGS_TABLE, LARVAL_DIPPING_EVENT, STRUCTURE, util);

        //recreate mosquito collection events and clients
        query = String.format("select * from %s where %s not in (select %s from %s ) ", MOSQUITO_COLLECTIONS_TABLE, BASE_ENTITY_ID, baseEntityId, event.name());
        Utils.recreateEventAndClients(query, new String[]{}, db, Utils.getFormTag(), MOSQUITO_COLLECTIONS_TABLE, MOSQUITO_COLLECTION_EVENT, STRUCTURE, util);


        //recreate poat events and clients
        query = String.format("select * from %s where %s not in (select %s from %s ) ", PAOT_TABLE, BASE_ENTITY_ID, baseEntityId, event.name());
        Utils.recreateEventAndClients(query, new String[]{}, db, Utils.getFormTag(), PAOT_TABLE, PAOT_EVENT, STRUCTURE, util);
    }

    private void upgradeToVersion5(SQLiteDatabase db) {
        if ((BuildConfig.BUILD_COUNTRY == Country.THAILAND || BuildConfig.BUILD_COUNTRY == Country.THAILAND_EN) && !isColumnExists(db, EVENT_TASK_TABLE, DatabaseKeys.PERSON_TESTED)) {
            db.execSQL(String.format("ALTER TABLE %s ADD COLUMN %s VARCHAR ", EVENT_TASK_TABLE, DatabaseKeys.PERSON_TESTED));
        }
    }

    private void upgradeToVersion6(SQLiteDatabase db) {
        ClientFormRepository.createTable(db);
        ManifestRepository.createTable(db);
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
        if (StringUtils.isBlank(password)) {
            throw new IllegalStateException("Password is blank");
        }
        try {
            if (readableDatabase == null || !readableDatabase.isOpen()) {
                readableDatabase = super.getReadableDatabase(password);
            }
            return readableDatabase;
        } catch (Exception e) {
            Timber.e(e, "Database Error. ");
            return null;
        }

    }

    @Override
    public synchronized SQLiteDatabase getWritableDatabase(String password) {
        if (StringUtils.isBlank(password)) {
            throw new IllegalStateException("Password is blank");
        } else if (writableDatabase == null || !writableDatabase.isOpen()) {
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
}
