package org.smartregister.reveal.interactor;

import com.mapbox.geojson.Feature;

import net.sqlcipher.Cursor;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.smartregister.commonregistry.CommonPersonObject;
import org.smartregister.commonregistry.CommonRepository;
import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.ListTaskContract;
import org.smartregister.reveal.model.CardDetails;
import org.smartregister.reveal.model.MosquitoHarvestCardDetails;
import org.smartregister.reveal.model.SprayCardDetails;
import org.smartregister.reveal.presenter.ListTaskPresenter;
import org.smartregister.reveal.util.CardDetailsUtil;
import org.smartregister.reveal.util.Constants.GeoJSON;
import org.smartregister.reveal.util.GeoJsonUtils;
import org.smartregister.reveal.util.Utils;

import java.util.List;
import java.util.Map;
import java.util.Set;

import timber.log.Timber;

import static org.smartregister.reveal.util.Constants.DatabaseKeys.BASE_ENTITY_ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.CODE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.LAST_UPDATED_DATE;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.PAOT_COMMENTS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.PAOT_STATUS;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.PLAN_ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.SPRAYED_STRUCTURES;
import static org.smartregister.reveal.util.Constants.Intervention.CASE_CONFIRMATION;
import static org.smartregister.reveal.util.Constants.Intervention.IRS;
import static org.smartregister.reveal.util.Constants.Intervention.LARVAL_DIPPING;
import static org.smartregister.reveal.util.Constants.Intervention.MOSQUITO_COLLECTION;
import static org.smartregister.reveal.util.Constants.Intervention.PAOT;
import static org.smartregister.reveal.util.Constants.Tables.LARVAL_DIPPINGS_TABLE;
import static org.smartregister.reveal.util.Constants.Tables.MOSQUITO_COLLECTIONS_TABLE;
import static org.smartregister.reveal.util.Constants.Tables.PAOT_TABLE;

/**
 * Created by samuelgithengi on 11/27/18.
 */
public class ListTaskInteractor extends BaseInteractor {


    private CommonRepository commonRepository;

    public ListTaskInteractor(ListTaskContract.Presenter presenter) {
        super(presenter);
        commonRepository = RevealApplication.getInstance().getContext().commonrepository(SPRAYED_STRUCTURES);
    }


    public void fetchInterventionDetails(String interventionType, String featureId, boolean isForForm) {
        String sql = "SELECT status, start_date, end_date FROM %s WHERE id=?";
        if (IRS.equals(interventionType)) {
            sql = "SELECT spray_status, not_sprayed_reason, not_sprayed_other_reason, property_type, spray_date," +
                    " spray_operator, family_head_name FROM sprayed_structures WHERE id=?";
        } else if (MOSQUITO_COLLECTION.equals(interventionType)) {
            sql = String.format(sql, MOSQUITO_COLLECTIONS_TABLE);
        } else if (LARVAL_DIPPING.equals(interventionType)) {
            sql = String.format(sql, LARVAL_DIPPINGS_TABLE);
        } else if (PAOT.equals(interventionType)) {
            sql = String.format("SELECT %s, %s, %s  FROM %s WHERE %s=? ", PAOT_STATUS,
                    PAOT_COMMENTS, LAST_UPDATED_DATE, PAOT_TABLE, BASE_ENTITY_ID);
        }

        final String SQL = sql;
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                Cursor cursor = getDatabase().rawQuery(SQL, new String[]{featureId});

                CardDetails cardDetails = null;
                try {
                    if (cursor.moveToFirst()) {
                        cardDetails = createCardDetails(cursor, interventionType);
                    }
                } catch (Exception e) {
                    Timber.e(e);
                } finally {
                    if (cursor != null) {
                        cursor.close();
                    }
                }

                // run on ui thread
                final CardDetails CARD_DETAILS = cardDetails;
                appExecutors.mainThread().execute(new Runnable() {
                    @Override
                    public void run() {
                        if (isForForm) {
                            ((ListTaskPresenter) presenterCallBack).onInterventionFormDetailsFetched(CARD_DETAILS);
                            getSprayDetails(interventionType, featureId, CARD_DETAILS);

                        } else {
                            ((ListTaskPresenter) presenterCallBack).onCardDetailsFetched(CARD_DETAILS);
                        }
                    }
                });
            }
        };

        appExecutors.diskIO().execute(runnable);
    }

    private void getSprayDetails(String interventionType, String structureId, CardDetails cardDetails) {
        if (IRS.equals(interventionType)) {
            Cursor cursor = null;
            try {
                cursor = eventClientRepository.getWritableDatabase().rawQuery(
                        String.format("select s.*, id as _id from %s s where %s = ?", SPRAYED_STRUCTURES, BASE_ENTITY_ID), new String[]{structureId});
                if (cursor.moveToFirst()) {
                    CommonPersonObject commonPersonObject = commonRepository.getCommonPersonObjectFromCursor(cursor);
                    ((SprayCardDetails) cardDetails).setCommonPersonObject(commonPersonObject);
                }
            } catch (Exception e) {
                Timber.e(e);
            } finally {
                if (cursor != null) {
                    cursor.close();
                }
            }
        }
    }

    private CardDetails createCardDetails(Cursor cursor, String interventionType) {
        CardDetails cardDetails = null;
        if (MOSQUITO_COLLECTION.equals(interventionType) || LARVAL_DIPPING.equals(interventionType)) {
            cardDetails = createMosquitoHarvestCardDetails(cursor, interventionType);
        } else if (IRS.equals(interventionType)) {
            cardDetails = createSprayCardDetails(cursor);
        } else if (PAOT.equals(interventionType)) {
            cardDetails = createPaotCardDetails(cursor, interventionType);
        }

        return cardDetails;
    }

    private SprayCardDetails createSprayCardDetails(Cursor cursor) {
        String reason = cursor.getString(cursor.getColumnIndex("not_sprayed_reason"));
        if ("other".equals(reason)) {
            reason = cursor.getString(cursor.getColumnIndex("not_sprayed_other_reason"));
        }
        return new SprayCardDetails(
                CardDetailsUtil.getTranslatedBusinessStatus(cursor.getString(cursor.getColumnIndex("spray_status"))),
                cursor.getString(cursor.getColumnIndex("property_type")),
                cursor.getString(cursor.getColumnIndex("spray_date")),
                cursor.getString(cursor.getColumnIndex("spray_operator")),
                cursor.getString(cursor.getColumnIndex("family_head_name")),
                reason
        );
    }

    private MosquitoHarvestCardDetails createMosquitoHarvestCardDetails(Cursor cursor, String interventionType) {
        return new MosquitoHarvestCardDetails(
                CardDetailsUtil.getTranslatedBusinessStatus(cursor.getString(cursor.getColumnIndex("status"))),
                cursor.getString(cursor.getColumnIndex("start_date")),
                cursor.getString(cursor.getColumnIndex("end_date")),
                interventionType
        );
    }

    private MosquitoHarvestCardDetails createPaotCardDetails(Cursor cursor, String interventionType) {
        MosquitoHarvestCardDetails paotCardDetails = new MosquitoHarvestCardDetails(
                cursor.getString(cursor.getColumnIndex(PAOT_STATUS)),
                cursor.getString(cursor.getColumnIndex(LAST_UPDATED_DATE)),
                null,
                interventionType
        );
        paotCardDetails.setComments(cursor.getString(cursor.getColumnIndex(PAOT_COMMENTS)));
        return paotCardDetails;
    }

    public void fetchLocations(String plan, String operationalArea) {
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                JSONObject featureCollection = null;
                Location operationalAreaLocation = Utils.getOperationalAreaLocation(operationalArea);
                try {
                    featureCollection = createFeatureCollection();
                    if (operationalAreaLocation != null) {
                        Map<String, Set<Task>> tasks = taskRepository.getTasksByPlanAndGroup(plan, operationalAreaLocation.getId());
                        List<Location> structures = structureRepository.getLocationsByParentId(operationalAreaLocation.getId());
                        String indexCase = getIndexCaseStructure(plan);
                        featureCollection.put(GeoJSON.FEATURES, new JSONArray(GeoJsonUtils.getGeoJsonFromStructuresAndTasks(structures, tasks, indexCase)));
                        Timber.d("features:" + featureCollection.toString());

                    }
                } catch (Exception e) {
                    Timber.e(e);
                }
                JSONObject finalFeatureCollection = featureCollection;
                appExecutors.mainThread().execute(new Runnable() {
                    @Override
                    public void run() {
                        if (operationalAreaLocation != null) {
                            operationalAreaId = operationalAreaLocation.getId();
                            Feature operationalAreaFeature = Feature.fromJson(gson.toJson(operationalAreaLocation));
                            getPresenter().onStructuresFetched(finalFeatureCollection, operationalAreaFeature);
                        } else {
                            getPresenter().onStructuresFetched(finalFeatureCollection, null);
                        }
                    }
                });
            }
        };

        appExecutors.diskIO().execute(runnable);
    }

    private String getIndexCaseStructure(String planId) {
        Cursor cursor = null;
        String structureId = null;
        try {
            cursor = getDatabase().rawQuery(getMemberTasksSelect(String.format("%s=? AND %s=?",
                    PLAN_ID, CODE), new String[]{}), new String[]{planId, CASE_CONFIRMATION});
            if (cursor.moveToNext()) {
                structureId = cursor.getString(0);
            }
        } catch (Exception e) {
            Timber.e(e);
        } finally {
            if (cursor != null) {
                cursor.close();
            }
        }
        return structureId;
    }

    private JSONObject createFeatureCollection() throws JSONException {
        JSONObject featureCollection = new JSONObject();
        featureCollection.put(GeoJSON.TYPE, GeoJSON.FEATURE_COLLECTION);
        return featureCollection;
    }

    private ListTaskContract.Presenter getPresenter() {
        return (ListTaskContract.Presenter) presenterCallBack;
    }
}
