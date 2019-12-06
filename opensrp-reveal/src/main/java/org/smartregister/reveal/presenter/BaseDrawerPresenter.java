package org.smartregister.reveal.presenter;

import android.support.v4.util.Pair;
import android.text.TextUtils;

import com.google.gson.reflect.TypeToken;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.domain.PlanDefinition;
import org.smartregister.domain.form.FormLocation;
import org.smartregister.location.helper.LocationHelper;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.interactor.BaseDrawerInteractor;
import org.smartregister.reveal.util.Country;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.util.AssetHandler;
import org.smartregister.util.Utils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import timber.log.Timber;

import static org.smartregister.AllConstants.OPERATIONAL_AREAS;
import static org.smartregister.reveal.util.Constants.PlanDefinitionStatus.ACTIVE;
import static org.smartregister.reveal.util.Constants.Tags.CANTON;
import static org.smartregister.reveal.util.Constants.Tags.COUNTRY;
import static org.smartregister.reveal.util.Constants.Tags.DISTRICT;
import static org.smartregister.reveal.util.Constants.Tags.HEALTH_CENTER;
import static org.smartregister.reveal.util.Constants.Tags.OPERATIONAL_AREA;
import static org.smartregister.reveal.util.Constants.Tags.PROVINCE;
import static org.smartregister.reveal.util.Constants.Tags.REGION;
import static org.smartregister.reveal.util.Constants.Tags.SUB_DISTRICT;
import static org.smartregister.reveal.util.Constants.Tags.VILLAGE;
import static org.smartregister.reveal.util.Constants.UseContextCode.INTERVENTION_TYPE;

/**
 * Created by samuelgithengi on 3/21/19.
 */
public class BaseDrawerPresenter implements BaseDrawerContract.Presenter {

    private BaseDrawerContract.View view;
    private BaseDrawerContract.DrawerActivity drawerActivity;

    private PreferencesUtil prefsUtil;

    private LocationHelper locationHelper;

    private boolean changedCurrentSelection;

    private BaseDrawerContract.Interactor interactor;


    private boolean viewInitialized = false;

    public BaseDrawerPresenter(BaseDrawerContract.View view, BaseDrawerContract.DrawerActivity drawerActivity) {
        this.view = view;
        this.drawerActivity = drawerActivity;
        this.prefsUtil = PreferencesUtil.getInstance();
        this.locationHelper = LocationHelper.getInstance();
        interactor = new BaseDrawerInteractor(this);
    }


    private void initializeDrawerLayout() {

        view.setOperator();

        if (StringUtils.isBlank(prefsUtil.getCurrentOperationalArea())) {
            ArrayList<String> operationalAreaLevels = new ArrayList<>();
            operationalAreaLevels.add(DISTRICT);
            operationalAreaLevels.add(HEALTH_CENTER);
            operationalAreaLevels.add(VILLAGE);
            operationalAreaLevels.add(CANTON);
            operationalAreaLevels.add(SUB_DISTRICT);
            List<String> defaultLocation = locationHelper.generateDefaultLocationHierarchy(operationalAreaLevels);

            if (defaultLocation != null) {
                view.setDistrict(defaultLocation.get(0));
                ArrayList<String> levels = new ArrayList<>();
                levels.add(CANTON);
                String level;
                if (locationHelper.generateLocationHierarchyTree(false, levels).isEmpty()) {
                    level = HEALTH_CENTER;
                } else {
                    level = CANTON;
                }
                if (defaultLocation.size() > 1)
                    view.setFacility(defaultLocation.get(1), level);
            }
        } else {
            populateLocationsFromPreferences();
        }

        view.setPlan(prefsUtil.getCurrentPlan());

    }


    @Override
    public void onPlansFetched(Set<PlanDefinition> planDefinitions) {
        List<String> ids = new ArrayList<>();
        List<FormLocation> formLocations = new ArrayList<>();
        for (PlanDefinition planDefinition : planDefinitions) {
            if (!planDefinition.getStatus().equals(ACTIVE)) {
                continue;
            }
            ids.add(planDefinition.getIdentifier());
            FormLocation formLocation = new FormLocation();
            formLocation.name = planDefinition.getTitle();
            formLocation.key = planDefinition.getIdentifier();
            formLocation.level = "";
            formLocations.add(formLocation);

            // get intervention type for plan
            for (PlanDefinition.UseContext useContext : planDefinition.getUseContext()) {
                if (useContext.getCode().equals(INTERVENTION_TYPE)) {
                    prefsUtil.setInterventionTypeForPlan(planDefinition.getTitle(), useContext.getValueCodableConcept());
                    break;
                }
            }

        }

        String entireTreeString = "";
        if (formLocations != null && !formLocations.isEmpty()) {
            entireTreeString = AssetHandler.javaToJsonString(formLocations,
                    new TypeToken<List<FormLocation>>() {
                    }.getType());
        }

        view.showPlanSelector(ids, entireTreeString);
    }

    private void populateLocationsFromPreferences() {
        view.setDistrict(prefsUtil.getCurrentDistrict());
        view.setFacility(prefsUtil.getCurrentFacility(), prefsUtil.getCurrentFacilityLevel());
        view.setOperationalArea(prefsUtil.getCurrentOperationalArea());
    }

    @Override
    public void onShowOperationalAreaSelector() {
        Pair<String, ArrayList<String>> locationHierarchy = extractLocationHierarchy();
        if (locationHierarchy == null) {//try to evict location hierachy in cache
            RevealApplication.getInstance().getContext().anmLocationController().evict();
            locationHierarchy = extractLocationHierarchy();
        }
        if (locationHierarchy != null) {
            view.showOperationalAreaSelector(extractLocationHierarchy());
        } else {
            view.displayNotification(R.string.error_fetching_location_hierarchy_title, R.string.error_fetching_location_hierarchy);
            RevealApplication.getInstance().getContext().userService().forceRemoteLogin();
        }

    }

    private Pair<String, ArrayList<String>> extractLocationHierarchy() {

        ArrayList<String> operationalAreaLevels = new ArrayList<>();
        operationalAreaLevels.add(COUNTRY);
        operationalAreaLevels.add(PROVINCE);
        operationalAreaLevels.add(REGION);
        operationalAreaLevels.add(DISTRICT);
        operationalAreaLevels.add(SUB_DISTRICT);
        operationalAreaLevels.add(OPERATIONAL_AREA);

        List<String> defaultLocation = locationHelper.generateDefaultLocationHierarchy(operationalAreaLevels);

        if (defaultLocation != null) {
            List<FormLocation> entireTree = locationHelper.generateLocationHierarchyTree(false, operationalAreaLevels);
            List<String> authorizedOperationalAreas = Arrays.asList(StringUtils.split(prefsUtil.getPreferenceValue(OPERATIONAL_AREAS), ','));
            removeUnauthorizedOperationalAreas(authorizedOperationalAreas, entireTree);

            String entireTreeString = AssetHandler.javaToJsonString(entireTree,
                    new TypeToken<List<FormLocation>>() {
                    }.getType());

            return new Pair<>(entireTreeString, new ArrayList<>(defaultLocation));
        } else {
            return null;
        }
    }


    public void onOperationalAreaSelectorClicked(ArrayList<String> name) {

        Timber.d("Selected Location Hierarchy: " + TextUtils.join(",", name));
        if (name.size() <= 2)//no operational area was selected, dialog was dismissed
            return;
        ArrayList<String> operationalAreaLevels = new ArrayList<>();
        operationalAreaLevels.add(DISTRICT);
        operationalAreaLevels.add(HEALTH_CENTER);
        operationalAreaLevels.add(SUB_DISTRICT);
        operationalAreaLevels.add(CANTON);
        operationalAreaLevels.add(OPERATIONAL_AREA);
        List<FormLocation> entireTree = locationHelper.generateLocationHierarchyTree(false, operationalAreaLevels);
        int districtOffset = name.get(0).equalsIgnoreCase(Country.BOTSWANA.name()) || name.get(0).equalsIgnoreCase(Country.NAMIBIA.name()) ? 3 : 2;
        try {
            prefsUtil.setCurrentProvince(name.get(1));
            prefsUtil.setCurrentDistrict(name.get(name.size() - districtOffset));
            prefsUtil.setCurrentOperationalArea(name.get(name.size() - 1));
            Pair<String, String> facility = getFacilityFromOperationalArea(name.get(name.size() - districtOffset), name.get(name.size() - 1), entireTree);
            if (facility != null) {
                prefsUtil.setCurrentFacility(facility.second);
                prefsUtil.setCurrentFacilityLevel(facility.first);
            }
        } catch (NullPointerException e) {
            Timber.e(e);
        }
        changedCurrentSelection = true;
        populateLocationsFromPreferences();
        unlockDrawerLayout();

    }


    private void removeUnauthorizedOperationalAreas(List<String> operationalAreas, List<FormLocation> entireTree) {

        for (FormLocation countryLocation : entireTree) {
            for (FormLocation provinceLocation : countryLocation.nodes) {
                if (provinceLocation.nodes == null)
                    return;
                for (FormLocation districtLocation : provinceLocation.nodes) {
                    if (districtLocation.nodes == null)
                        return;
                    List<FormLocation> toRemove = new ArrayList<>();
                    for (FormLocation operationalAreaLocation : districtLocation.nodes) {
                        if (!operationalAreas.contains(operationalAreaLocation.name))
                            toRemove.add(operationalAreaLocation);
                    }
                    districtLocation.nodes.removeAll(toRemove);
                }
            }
        }
    }

    private Pair<String, String> getFacilityFromOperationalArea(String district, String operationalArea, List<FormLocation> entireTree) {
        for (FormLocation districtLocation : entireTree) {
            if (!districtLocation.name.equals(district))
                continue;
            for (FormLocation facilityLocation : districtLocation.nodes) {
                for (FormLocation operationalAreaLocation : facilityLocation.nodes) {
                    if (operationalAreaLocation.name.equals(operationalArea)) {
                        return new Pair<>(facilityLocation.level, facilityLocation.name);
                    }
                }
            }
        }
        return null;
    }

    @Override
    public void onShowPlanSelector() {
        if (StringUtils.isBlank(prefsUtil.getCurrentOperationalArea())) {
            view.displayNotification(R.string.operational_area, R.string.operational_area_not_selected);
        } else {
            interactor.fetchPlans(prefsUtil.getCurrentOperationalArea());
        }
    }


    public void onPlanSelectorClicked(ArrayList<String> value, ArrayList<String> name) {
        if (Utils.isEmptyCollection(name) || (name.size() > 1))
            return;
        Timber.d("Selected Plan : " + TextUtils.join(",", name));
        Timber.d("Selected Plan Ids: " + TextUtils.join(",", value));

        prefsUtil.setCurrentPlan(name.get(0));
        prefsUtil.setCurrentPlanId(value.get(0));
        view.setPlan(name.get(0));
        changedCurrentSelection = true;
        unlockDrawerLayout();

    }

    public void onDrawerClosed() {
        drawerActivity.onDrawerClosed();
    }

    private void unlockDrawerLayout() {
        if (isPlanAndOperationalAreaSelected()) {
            view.unlockNavigationDrawer();
        }

    }

    @Override
    public boolean isChangedCurrentSelection() {
        return changedCurrentSelection;
    }

    @Override
    public void setChangedCurrentSelection(boolean changedCurrentSelection) {
        this.changedCurrentSelection = changedCurrentSelection;
    }

    @Override
    public BaseDrawerContract.View getView() {
        return view;
    }

    @Override
    public void onViewResumed() {
        if (viewInitialized) {
            if (!prefsUtil.getCurrentPlan().equals(view.getPlan())
                    || !prefsUtil.getCurrentOperationalArea().equals(view.getOperationalArea())) {
                changedCurrentSelection = true;
                onDrawerClosed();
            }
        } else {
            initializeDrawerLayout();
            viewInitialized = true;
        }
    }


    @Override
    public boolean isPlanAndOperationalAreaSelected() {
        String planId = PreferencesUtil.getInstance().getCurrentPlanId();
        String operationalArea = PreferencesUtil.getInstance().getCurrentOperationalArea();

        return StringUtils.isNotBlank(planId) && StringUtils.isNotBlank(operationalArea);

    }

}
