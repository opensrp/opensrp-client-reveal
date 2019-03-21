package org.smartregister.reveal.contract;

import android.content.Context;
import android.support.annotation.StringRes;
import android.support.v4.util.Pair;
import android.support.v7.app.AppCompatActivity;

import org.smartregister.domain.Campaign;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by samuelgithengi on 3/21/19.
 */
public class BaseDrawerContract {

    public interface DrawerActivity {
        void onDrawerClosed();

        AppCompatActivity getActivity();
    }

    public interface View {

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

        void onInitializeDrawerLayout();

        Presenter getPresenter();
    }

    public interface Presenter {

        void onDrawerClosed();

        void onShowOperationalAreaSelector();

        void onOperationalAreaSelectorClicked(ArrayList<String> name);

        void onShowCampaignSelector();

        void onCampaignSelectorClicked(ArrayList<String> value, ArrayList<String> name);

        void onInitializeDrawerLayout();

        void onCampaignsFetched(List<Campaign> campaigns);

        boolean isChangedCurrentSelection();

        void setChangedCurrentSelection(boolean changedCurrentSelection);

        View getView();
    }

    public interface Interactor {

        void fetchCampaigns();
    }
}
