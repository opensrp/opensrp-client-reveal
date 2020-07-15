package org.smartregister.reveal.view;

import android.app.Activity;
import android.content.Intent;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.widget.Toast;

import androidx.viewpager.widget.ViewPager;

import org.apache.commons.lang3.StringUtils;
import org.json.JSONObject;
import org.smartregister.domain.FetchStatus;
import org.smartregister.domain.Task;
import org.smartregister.family.activity.BaseFamilyProfileActivity;
import org.smartregister.family.adapter.ViewPagerAdapter;
import org.smartregister.family.util.Constants;
import org.smartregister.family.util.Constants.INTENT_KEY;
import org.smartregister.family.util.JsonFormUtils;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.FamilyProfileContract;
import org.smartregister.reveal.fragment.FamilyProfileMemberFragment;
import org.smartregister.reveal.fragment.StructureTasksFragment;
import org.smartregister.reveal.model.FamilyProfileModel;
import org.smartregister.reveal.presenter.FamilyProfilePresenter;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.util.FamilyConstants;

import timber.log.Timber;

import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURE_ID;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.TASK_ID;
import static org.smartregister.reveal.util.Constants.RequestCode.REQUEST_CODE_GET_JSON_FRAGMENT;

/**
 * Created by samuelgithengi on 2/8/19.
 */
public class FamilyProfileActivity extends BaseFamilyProfileActivity implements FamilyProfileContract.View {

    private StructureTasksFragment structureTasksFragment;

    private FamilyProfileMemberFragment profileMemberFragment;

    private String familyBaseEntityId;

    private String familyName;


    @Override
    protected void initializePresenter() {
        familyBaseEntityId = getIntent().getStringExtra(INTENT_KEY.FAMILY_BASE_ENTITY_ID);
        String familyHead = getIntent().getStringExtra(INTENT_KEY.FAMILY_HEAD);
        String primaryCaregiver = getIntent().getStringExtra(INTENT_KEY.PRIMARY_CAREGIVER);
        familyName = getIntent().getStringExtra(INTENT_KEY.FAMILY_NAME);

        presenter = new FamilyProfilePresenter(this,
                new FamilyProfileModel(familyName), familyBaseEntityId, familyHead, primaryCaregiver, familyName);
    }

    @Override
    protected ViewPager setupViewPager(ViewPager viewPager) {
        adapter = new ViewPagerAdapter(getSupportFragmentManager());

        profileMemberFragment = FamilyProfileMemberFragment.newInstance(this.getIntent().getExtras());
        adapter.addFragment(profileMemberFragment, this.getString(R.string.residents).toUpperCase());

        structureTasksFragment = StructureTasksFragment.newInstance(this.getIntent().getExtras(), this);
        adapter.addFragment(structureTasksFragment, this.getString(R.string.tasks, 0).toUpperCase());

        viewPager.setAdapter(adapter);

        return viewPager;
    }

    @Override
    public void setProfileImage(String baseEntityId) {
        //do nothing
    }

    @Override
    public void setStructureId(String structureId) {
        if (BuildConfig.BUILD_COUNTRY.equals(Country.NTD_COMMUNITY)) {
            structureTasksFragment.setStructure(null, familyBaseEntityId);
        } else {
            structureTasksFragment.setStructure(structureId, null);
        }
        profileMemberFragment.setStructure(structureId);
    }

    @Override
    public void refreshTasks(String structureId) {
        if (BuildConfig.BUILD_COUNTRY.equals(Country.NTD_COMMUNITY)) {
            structureTasksFragment.refreshTasks(null, familyBaseEntityId);
        } else {
            structureTasksFragment.refreshTasks(structureId, null);
        }
    }

    @Override
    public void updateFamilyName(String firstName) {
        if (profileMemberFragment.getArguments() != null) {
            profileMemberFragment.getArguments().putString(INTENT_KEY.FAMILY_NAME, firstName);
        }
    }

    @Override
    public Activity getContext() {
        return this;
    }

    @Override
    public void returnToMapView(String structureId, Task task) {
        Intent result = new Intent();
        result.putExtra(STRUCTURE_ID, structureId);
        result.putExtra(TASK_ID, task);
        setResult(RESULT_OK, result);
        finish();
    }

    @Override
    public void setLoadingState(boolean state) {
        if (state) {
            showProgressDialog(R.string.please_wait);
        } else {
            hideProgressDialog();
        }
    }

    @Override
    public void onError(Exception e) {

        Toast.makeText(getBaseContext(), R.string.an_error_occured, Toast.LENGTH_SHORT).show();
        if (e instanceof IllegalAccessException) {
            Toast.makeText(getBaseContext(), e.getMessage(), Toast.LENGTH_SHORT).show();
        }

        Timber.e(e);
    }

    @Override
    public void refreshViews(String structureId) {
        if (StringUtils.isNotBlank(structureId))
            refreshTasks(structureId);
        refreshMemberList(FetchStatus.fetched);
    }


    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (BuildConfig.BUILD_COUNTRY.equals(Country.NTD_COMMUNITY)) {
            if (requestCode == JsonFormUtils.REQUEST_CODE_GET_JSON && resultCode == RESULT_OK) {
                try {
                    String jsonString = data.getStringExtra(Constants.JSON_FORM_EXTRA.JSON);
                    Timber.d(jsonString);
                    JSONObject form = new JSONObject(jsonString);
                    if (form.getString(JsonFormUtils.ENCOUNTER_TYPE).equals(FamilyConstants.EventType.FAMILY_MEMBER_REGISTRATION)) {
                        ((FamilyProfilePresenter) presenter()).saveFamilyMember(getContext(), form, familyBaseEntityId, familyName);
                    } else if (form.getString(JsonFormUtils.ENCOUNTER_TYPE).equals(FamilyConstants.EventType.UPDATE_FAMILY_MEMBER_REGISTRATION)) {
                        ((FamilyProfilePresenter) presenter()).updateFamilyMember(form, familyBaseEntityId, familyName);
                    } else {
                        super.onActivityResult(requestCode, resultCode, data);
                    }
                } catch (Exception e) {
                    Timber.e(e);
                }
            } else if (requestCode == REQUEST_CODE_GET_JSON_FRAGMENT && resultCode == RESULT_OK) {
                structureTasksFragment.onActivityResult(requestCode, resultCode, data);
            } else {
                super.onActivityResult(requestCode, resultCode, data);
            }

        } else if (requestCode == io.ona.kujaku.utils.Constants.RequestCode.LOCATION_SETTINGS ||
                requestCode == REQUEST_CODE_GET_JSON_FRAGMENT) {
            structureTasksFragment.onActivityResult(requestCode, resultCode, data);
        } else {
            super.onActivityResult(requestCode, resultCode, data);
        }
    }

    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = this.getMenuInflater();
        inflater.inflate(R.menu.profile_menu, menu);
        if (BuildConfig.BUILD_COUNTRY.equals(Country.NTD_COMMUNITY)) {
            menu.findItem(R.id.edit_family).setVisible(false);
            menu.findItem(R.id.archive_family).setVisible(false);
        }
        return true;
    }

    public boolean onOptionsItemSelected(MenuItem item) {
        if (R.id.edit_family == item.getItemId()) {
            startFormForEdit();
            return true;
        } else if (R.id.add_member == item.getItemId()) {
            presenter().onAddFamilyMember();
            return true;
        } else if (R.id.archive_family == item.getItemId()) {
            presenter().onArchiveFamilyClicked();
            return true;
        } else {
            return super.onOptionsItemSelected(item);
        }
    }

    public void startFormForEdit() {
        if (familyBaseEntityId != null) {
            presenter().fetchProfileData();
        }
    }

    @Override
    public FamilyProfileContract.Presenter presenter() {
        return (FamilyProfileContract.Presenter) super.presenter();
    }
}
