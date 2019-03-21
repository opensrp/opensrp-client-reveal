package org.smartregister.reveal.contract;

import android.content.Context;
import android.support.annotation.StringRes;
import android.support.v4.util.Pair;

import org.smartregister.domain.Campaign;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by samuelgithengi on 3/21/19.
 */
public class BaseDrawerContract {

    public interface View {

        void showProgressDialog(@StringRes int title, @StringRes int message);

        void hideProgressDialog();

        Context getContext();

        void setCampaign(String campaign);

        void setOperationalArea(String operationalArea);

        void setDistrict(String district);

        void setFacility(String facility);

        void setOperator();

        void unlockNavigationDrawer();

        void lockNavigationDrawerForSelection();

        void showOperationalAreaSelector(Pair<String, ArrayList<String>> locationHierarchy);

        void showCampaignSelector(List<String> campaigns, String entireTreeString);

        void displayNotification(int title, @StringRes int message, Object... formatArgs);

        void displayNotification(String message);

    }

    public interface Presenter {

        void onDrawerClosed();

        void onOperationalAreaSelectorClicked(ArrayList<String> name);

        void onCampaignSelectorClicked(ArrayList<String> value, ArrayList<String> name);

        void onCampaignsFetched(List<Campaign> campaigns);
    }

    public interface Interactor {

        void fetchCampaigns();
    }
}
