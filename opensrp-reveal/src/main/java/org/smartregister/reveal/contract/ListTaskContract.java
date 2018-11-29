package org.smartregister.reveal.contract;

import android.content.Context;
import android.support.v4.util.Pair;

import org.smartregister.domain.Campaign;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by samuelgithengi on 11/27/18.
 */
public interface ListTaskContract {

    interface ListTaskView {

        Context getContext();

        void showOperationalAreaSelector(Pair<String, ArrayList<String>> locationHierarchy);

        void setCampaign(String campaign);

        void setOperationalArea(String operationalArea);

        void setDistrict(String district);

        void setFacility(String facility);

        void setOperator();

        void showCampaignSelector(List<String> campaigns, String entireTreeString);
    }

    interface PresenterCallBack {

        void onCampaignsFetched(List<Campaign> campaigns);
    }
}
