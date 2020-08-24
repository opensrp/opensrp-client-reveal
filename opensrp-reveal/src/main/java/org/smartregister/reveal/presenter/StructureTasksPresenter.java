package org.smartregister.reveal.presenter;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.annotation.VisibleForTesting;
import androidx.core.util.Pair;

import com.mapbox.geojson.Feature;

import org.apache.commons.lang3.StringUtils;
import org.joda.time.DateTime;
import org.json.JSONArray;
import org.json.JSONObject;
import org.smartregister.commonregistry.CommonPersonObjectClient;
import org.smartregister.domain.Event;
import org.smartregister.domain.Location;
import org.smartregister.domain.Task;
import org.smartregister.domain.Task.TaskStatus;
import org.smartregister.repository.BaseRepository;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.BaseFormFragmentContract;
import org.smartregister.reveal.contract.StructureTasksContract;
import org.smartregister.reveal.dao.ReportDao;
import org.smartregister.reveal.dao.StructureDao;
import org.smartregister.reveal.dao.TaskDetailsDao;
import org.smartregister.reveal.interactor.BaseFormFragmentInteractor;
import org.smartregister.reveal.interactor.StructureTasksInteractor;
import org.smartregister.reveal.model.MDAOutCome;
import org.smartregister.reveal.model.StructureTaskDetails;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.util.NativeFormProcessor;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.TaskUtils;
import org.smartregister.util.CallableInteractor;
import org.smartregister.util.CallableInteractorCallBack;
import org.smartregister.util.GenericInteractor;

import java.lang.ref.WeakReference;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Callable;

import timber.log.Timber;

import static org.smartregister.reveal.contract.StructureTasksContract.Interactor;
import static org.smartregister.reveal.contract.StructureTasksContract.Presenter;
import static org.smartregister.reveal.util.Constants.Intervention.BLOOD_SCREENING;

/**
 * Created by samuelgithengi on 4/12/19.
 */
public class StructureTasksPresenter extends BaseFormFragmentPresenter implements Presenter {

    private WeakReference<StructureTasksContract.View> view;

    private Interactor interactor;

    private PreferencesUtil prefsUtil;

    private StructureTaskDetails indexCase;

    private String structureId;

    private String familyBaseEntityId;

    private BaseFormFragmentContract.Interactor formInteractor;


    public StructureTasksPresenter(StructureTasksContract.View view, Context context) {
        this(view, context, null, PreferencesUtil.getInstance());
        interactor = new StructureTasksInteractor(this);
        formInteractor = new BaseFormFragmentInteractor(this);
    }

    @VisibleForTesting
    public StructureTasksPresenter(StructureTasksContract.View view, Context context, Interactor interactor, PreferencesUtil prefsUtil) {
        super(view, context);
        this.view = new WeakReference<>(view);
        this.interactor = interactor;
        this.prefsUtil = prefsUtil;
    }

    @Override
    public void findTasks(String structureId, String familyBaseEntityId) {
        this.structureId = structureId;
        this.familyBaseEntityId = familyBaseEntityId;
        if (StringUtils.isNotBlank(structureId)) {
            interactor.findTasks(structureId, prefsUtil.getCurrentPlanId(),
                    prefsUtil.getCurrentOperationalAreaId());
        } else if (StringUtils.isNotBlank(familyBaseEntityId)) {
            interactor.findTasks(familyBaseEntityId);
        }
    }

    @Override
    public void refreshTasks() {
        if (structureId != null)
            findTasks(structureId, null);

        if (familyBaseEntityId != null)
            findTasks(null, familyBaseEntityId);
    }


    @Override
    public void onTasksFound(List<StructureTaskDetails> taskDetailsList, StructureTaskDetails incompleteIndexCase) {
        indexCase = incompleteIndexCase;
        getView().setTaskDetailsList(taskDetailsList);
        if (incompleteIndexCase != null) {
            getView().displayDetectCaseButton();
        }
    }

    @Override
    public void onTaskSelected(StructureTaskDetails details, boolean isEdit, boolean isUndo) {
        if (details != null) {

            if (BuildConfig.BUILD_COUNTRY.equals(Country.NTD_COMMUNITY)) {
                startMDAForm(details.getPersonBaseEntityId());
                return;
            }

            if (TaskStatus.COMPLETED.name().equals(details.getTaskStatus())) {
                if (isEdit) {
                    details.setEdit(true);
                    getView().showProgressDialog(R.string.opening_form_title, R.string.opening_form_message);
                    interactor.getStructure(details);
                } else if (isUndo) {
                    getView().displayResetTaskInfoDialog(details);
                } else {
                    getView().displayToast("Task Completed");
                }
            } else {
                getView().showProgressDialog(R.string.opening_form_title, R.string.opening_form_message);
                interactor.getStructure(details);
            }
        }
    }

    public void startMDAForm(String baseEntityID) {
        Context context = RevealApplication.getInstance().getContext().applicationContext();

        CallableInteractor myInteractor = new GenericInteractor();

        Callable<JSONObject> callable = () -> {

            String jsonForm = org.smartregister.util.Utils.readAssetContents(context, Constants.JsonForm.NTD_COMMUNITY_MASS_DRUG_ADMINISTRATION);
            JSONObject jsonObject = new JSONObject(jsonForm);
            jsonObject.put(Constants.Properties.BASE_ENTITY_ID, baseEntityID);

            return jsonObject;
        };

        getView().showProgressDialog(R.string.opening_form_title, R.string.opening_form_message);
        myInteractor.execute(callable, new CallableInteractorCallBack<JSONObject>() {
            @Override
            public void onResult(JSONObject jsonObject) {
                getView().startForm(jsonObject);
                getView().hideProgressDialog();
            }

            @Override
            public void onError(Exception ex) {
                Timber.e(ex);
                getView().hideProgressDialog();
            }
        });
    }

    public void saveMDAForm(String jsonString) {
        CallableInteractor myInteractor = new GenericInteractor();

        Callable<List<StructureTaskDetails>> callable = () -> {

            String entityId = new JSONObject(jsonString).getString(Constants.Properties.BASE_ENTITY_ID);

            // update metadata
            NativeFormProcessor processor = NativeFormProcessor.createInstance(jsonString)
                    .withBindType(Constants.EventType.MDA_DISPENSE)
                    .withEncounterType(Constants.EventType.MDA_DISPENSE)
                    .withEntityId(entityId);

            // get task
            Task task = TaskDetailsDao.getInstance().getCurrentTask(entityId, Constants.Intervention.NTD_MDA_DISPENSE);

            // update the task
            boolean completed = processor.getFieldValue("nPzqDistribute").equalsIgnoreCase("Yes");

            task.setBusinessStatus(completed ?
                    Constants.BusinessStatus.VISITED_DRUG_ADMINISTERED :
                    Constants.BusinessStatus.VISITED_DRUG_NOT_ADMINISTERED
            );

            task.setStatus(Task.TaskStatus.COMPLETED);
            if (BaseRepository.TYPE_Synced.equals(task.getSyncStatus())) {
                task.setSyncStatus(BaseRepository.TYPE_Unsynced);
            }
            task.setLastModified(new DateTime());
            RevealApplication.getInstance().getTaskRepository().addOrUpdate(task);

            // save event details
            Location operationalArea = processor.getCurrentOperationalArea();
            processor
                    .tagLocationData(operationalArea)
                    .tagTaskDetails(task)
                    .tagEventMetadata()

                    // save and clientM
                    .saveEvent()
                    .clientProcessForm();


            StructureDao structureDao = StructureDao.getInstance();
            Pair<String, String> result = structureDao.getFamilyIdAndStructureIdByMemberId(entityId);

            ReportDao reportDao = ReportDao.getInstance();
            MDAOutCome mdaOutCome = reportDao.calculateFamilyMDA(result.first, prefsUtil.getCurrentPlanId(), prefsUtil.getCurrentOperationalAreaId());

            MDAOutCome.MDAOutComeStatus mdaOutComeStatus = mdaOutCome.getStatus();

            String status;
            switch (mdaOutComeStatus) {
                case PARTIAL:
                    status = Constants.BusinessStatus.VISITED_PARTIALLY_TREATED;
                    break;
                case POSITIVE:
                    status = Constants.BusinessStatus.COMPLETE;
                    break;
                default:
                    status = Constants.BusinessStatus.VISITED_NOT_TREATED;
            }

            TaskUtils taskUtils = TaskUtils.getInstance();
            if (StringUtils.isNotBlank(result.second)) {
                taskUtils.updateTaskStatus(
                        result.second,
                        Constants.Intervention.STRUCTURE_VISITED,
                        status,
                        Task.TaskStatus.COMPLETED
                );
            } else {
                // floating family MDA

                // floating families task
                taskUtils.updateTaskStatus(
                        result.first,
                        Constants.Intervention.FLOATING_FAMILY_REGISTRATION,
                        status,
                        Task.TaskStatus.COMPLETED
                );
                /*
                Task floatingRegistration = TaskDetailsDao.getInstance().getCurrentTask(result.first, Constants.Intervention.FLOATING_FAMILY_REGISTRATION);
                floatingRegistration.setBusinessStatus(status);
                floatingRegistration.setStatus(Task.TaskStatus.COMPLETED);

                if (BaseRepository.TYPE_Synced.equals(floatingRegistration.getSyncStatus())) {
                    floatingRegistration.setSyncStatus(BaseRepository.TYPE_Unsynced);
                }
                floatingRegistration.setLastModified(new DateTime());
                RevealApplication.getInstance().getTaskRepository().addOrUpdate(floatingRegistration);

                 */
            }

            return TaskDetailsDao.getInstance().getFamilyStructureTasks(familyBaseEntityId);
        };


        getView().showProgressDialog(R.string.saving_title, R.string.saving_message);
        myInteractor.execute(callable, new CallableInteractorCallBack<List<StructureTaskDetails>>() {
            @Override
            public void onResult(List<StructureTaskDetails> result) {
                getView().setTaskDetailsList(result);
                getView().hideProgressDialog();
            }

            @Override
            public void onError(Exception ex) {
                Timber.e(ex);
                getView().hideProgressDialog();
            }
        });
    }

    @Override
    public void onLocationValidated() {
        StructureTaskDetails taskDetails = (StructureTaskDetails) getTaskDetails();
        if (taskDetails.isEdit() && (Constants.Intervention.BEDNET_DISTRIBUTION.equals(taskDetails.getTaskCode()) || BLOOD_SCREENING.equals(taskDetails.getTaskCode()))) {
            interactor.findLastEvent(taskDetails);
        } else {
            super.onLocationValidated();
        }
    }

    public StructureTasksContract.View getView() {
        return view.get();
    }

    @Override
    public void saveJsonForm(String json) {
        getView().showProgressDialog(R.string.saving_title, R.string.saving_message);
        interactor.saveJsonForm(json);
    }

    @Override
    public void onDetectCase() {
        indexCase.setStructureId(structureId);
        interactor.getStructure(indexCase);
    }

    @Override
    public void onIndexConfirmationFormSaved(String taskID, TaskStatus taskStatus, String businessStatus, Set<Task> removedTasks) {
        getView().updateTasks(taskID, taskStatus, businessStatus, removedTasks);
        if (taskStatus.equals(TaskStatus.COMPLETED)) {
            getView().hideDetectCaseButton();
        }
        if (!removedTasks.isEmpty()) {
            getView().updateNumberOfTasks();
        }
        getView().hideProgressDialog();
    }

    @Override
    public void onEventFound(Event event) {

        String formName = getView().getJsonFormUtils().getFormName(null, getTaskDetails().getTaskCode());
        if (StringUtils.isBlank(formName)) {
            getView().displayError(R.string.opening_form_title, R.string.form_not_found);
        } else {
            JSONObject formJSON = getView().getJsonFormUtils().getFormJSON(getView().getContext(), formName, getTaskDetails(), getStructure());
            getView().getJsonFormUtils().populateForm(event, formJSON);
            if (Constants.Intervention.BEDNET_DISTRIBUTION.equals(getTaskDetails().getTaskCode())) {
                formInteractor.findNumberOfMembers(getTaskDetails().getTaskEntity(), formJSON);
            } else {
                getView().startForm(formJSON);
            }
        }
        getView().hideProgressDialog();

    }

    @Override
    public void resetTaskInfo(StructureTaskDetails taskDetails) {
        interactor.resetTaskInfo(getView().getContext(), taskDetails);
    }

    @Override
    public void onTaskInfoReset(String structureId) {
        findTasks(structureId, null);
    }

    @Override
    public void onFormSaved(@NonNull String structureId, String taskID, @NonNull TaskStatus taskStatus, @NonNull String businessStatus, String interventionType) {
        getView().hideProgressDialog();
        getView().updateTask(taskID, taskStatus, businessStatus);
    }

    @Override
    public void onStructureAdded(Feature feature, JSONArray featureCoordinates, double zoomlevel) {//not used
    }

    @Override
    public void onFormSaveFailure(String eventType) {
        getView().hideProgressDialog();//register will refresh on resume
    }

    @Override
    public void onFamilyFound(CommonPersonObjectClient finalFamily) {//not used
    }
}
