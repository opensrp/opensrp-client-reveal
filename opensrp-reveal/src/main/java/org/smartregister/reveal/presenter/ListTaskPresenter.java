package org.smartregister.reveal.presenter;

import android.support.v4.util.Pair;

import com.google.gson.reflect.TypeToken;

import org.smartregister.domain.form.FormLocation;
import org.smartregister.location.helper.LocationHelper;
import org.smartregister.reveal.contract.ListTaskView;
import org.smartregister.reveal.interactor.ListTaskInteractor;
import org.smartregister.util.AssetHandler;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by samuelgithengi on 11/27/18.
 */
public class ListTaskPresenter {

    private ListTaskView listTaskView;

    private ListTaskInteractor listTaskInteractor;

    public ListTaskPresenter(ListTaskView listTaskView) {
        this.listTaskView = listTaskView;
        listTaskInteractor = new ListTaskInteractor();
    }

    public void onInitializeDrawerLayout() {
        listTaskView.setCampaign("IRS Season 1 2018");
        listTaskView.setOperationalArea("01_157");
        listTaskView.setDistrict("Chadiza");
        listTaskView.setFacility("Chanjobwe Clinic");
        listTaskView.setOperator();

    }

    public Pair<String, ArrayList<String>> processLocationHierarchy() {
        ArrayList<String> operationalAreaLevels = new ArrayList<>();
        operationalAreaLevels.add("Country");
        operationalAreaLevels.add("Province");
        operationalAreaLevels.add("District");
        operationalAreaLevels.add("Operational Area");

        List<String> defaultLocation = LocationHelper.getInstance().generateDefaultLocationHierarchy(operationalAreaLevels);

        List<FormLocation> entireTree = LocationHelper.getInstance().generateLocationHierarchyTree(false, operationalAreaLevels);


        String entireTreeString = AssetHandler.javaToJsonString(entireTree,
                new TypeToken<List<FormLocation>>() {
                }.getType());

        return new Pair<>(entireTreeString, new ArrayList<>(defaultLocation));
    }

    public void onCampaignSelectorClicked() {
    }
}
