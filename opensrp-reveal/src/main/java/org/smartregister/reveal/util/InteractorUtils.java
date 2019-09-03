package org.smartregister.reveal.util;

import android.database.Cursor;

import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.repository.EventClientRepository;

import timber.log.Timber;

import static org.smartregister.reveal.util.Constants.DatabaseKeys.SPRAYED_STRUCTURES;
import static org.smartregister.reveal.util.Constants.Intervention.IRS;

public class InteractorUtils {

    public CommonPersonObject fetchSprayDetails(String interventionType, String structureId, EventClientRepository eventClientRepository, CommonRepository commonRepository) {
        CommonPersonObject commonPersonObject = null;
        if (IRS.equals(interventionType)) {
            Cursor cursor = null;
            try {
                cursor = eventClientRepository.getWritableDatabase().rawQuery(
                        String.format("select s.*, id as _id from %s s where %s = ?", SPRAYED_STRUCTURES, Constants.DatabaseKeys.BASE_ENTITY_ID), new String[]{structureId});
                if (cursor.moveToFirst()) {
                    commonPersonObject = commonRepository.getCommonPersonObjectFromCursor(cursor);
                }
            } catch (Exception e) {
                Timber.e(e);
            } finally {
                if (cursor != null) {
                    cursor.close();
                }
            }
        }
        return commonPersonObject;
    }
}
