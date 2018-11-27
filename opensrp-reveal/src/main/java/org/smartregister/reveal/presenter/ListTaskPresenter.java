package org.smartregister.reveal.presenter;

import org.smartregister.reveal.contract.ListTaskView;

/**
 * Created by samuelgithengi on 11/27/18.
 */
public class ListTaskPresenter {

    private ListTaskView listTaskView;

    public ListTaskPresenter(ListTaskView listTaskView) {
        this.listTaskView = listTaskView;
    }

    public void onInitializeDrawerLayout() {
        listTaskView.setCampaign("IRS Season 1 2018");
        listTaskView.setOperationalArea("01_157");
        listTaskView.setDistrict("Chadiza");
        listTaskView.setFacility("Chanjobwe Clinic");

    }

}
