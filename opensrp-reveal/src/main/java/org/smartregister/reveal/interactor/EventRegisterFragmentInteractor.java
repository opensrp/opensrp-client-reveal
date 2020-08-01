package org.smartregister.reveal.interactor;

import android.database.Cursor;
import android.database.SQLException;

import net.sqlcipher.database.SQLiteDatabase;

import org.smartregister.repository.EventClientRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.EventRegisterContract;
import org.smartregister.reveal.util.AppExecutors;

import timber.log.Timber;

/**
 * Created by Richard Kareko on 7/31/20.
 */

public class EventRegisterFragmentInteractor implements EventRegisterContract.Interactor {

    private EventRegisterContract.Presenter presenter;

    private EventClientRepository eventClientRepository;

    private AppExecutors appExecutors;

    private SQLiteDatabase database;

    public EventRegisterFragmentInteractor(EventRegisterContract.Presenter presenter) {
        this.presenter = presenter;
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        database = RevealApplication.getInstance().getRepository().getReadableDatabase();
        eventClientRepository = RevealApplication.getInstance().getContext().getEventClientRepository();
    }

    @Override
    public void findEvent(String formSubmissionId) {
        appExecutors.diskIO().execute(() -> {
            String eventSql = String.format("select %s from %s where %s = ? ",
                    EventClientRepository.event_column.json, EventClientRepository.Table.event.name(), EventClientRepository.event_column.formSubmissionId);

            try (Cursor cursor = database.rawQuery(eventSql, new String[]{formSubmissionId})) {

                if (cursor.moveToFirst()) {
                    String eventJSON = cursor.getString(0);

                    appExecutors.mainThread().execute(() -> {
                       presenter.onEventFound(eventClientRepository.convert(eventJSON, org.smartregister.domain.Event.class));
                    });

                }
            } catch (SQLException e) {
                Timber.e(e);
            }
        });

    }
}
