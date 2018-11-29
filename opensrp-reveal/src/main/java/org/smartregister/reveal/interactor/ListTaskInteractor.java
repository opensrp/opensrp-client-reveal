package org.smartregister.reveal.interactor;

import android.support.annotation.VisibleForTesting;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import org.joda.time.DateTime;
import org.joda.time.LocalDate;
import org.smartregister.domain.Campaign;
import org.smartregister.repository.CampaignRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.ListTaskContract.PresenterCallBack;
import org.smartregister.reveal.util.AppExecutors;
import org.smartregister.util.DateTimeTypeConverter;
import org.smartregister.util.DateTypeConverter;

import java.util.List;

/**
 * Created by samuelgithengi on 11/27/18.
 */
public class ListTaskInteractor {

    private AppExecutors appExecutors;

    private CampaignRepository campaignRepository;

    public ListTaskInteractor() {
        this(new AppExecutors());
        this.campaignRepository = new CampaignRepository(RevealApplication.getInstance().getRepository());
    }

    @VisibleForTesting
    protected ListTaskInteractor(AppExecutors appExecutors) {
        this.appExecutors = appExecutors;
    }

    public void fetchCampaigns(PresenterCallBack callBack) {
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                //TODO remove method call once sync is implemented
                generateDummyCampaign();
                List<Campaign> campaigns = campaignRepository.getCampaignsAllCampaigns();
                appExecutors.mainThread().execute(new Runnable() {
                    @Override
                    public void run() {
                        callBack.onCampaignsFetched(campaigns);
                    }
                });

            }
        };

        appExecutors.diskIO().execute(runnable);
    }


    /**
     * TODO remove once sync is implemented
     */
    private void generateDummyCampaign() {
        try {
            Gson gson = new GsonBuilder().registerTypeAdapter(DateTime.class, new DateTimeTypeConverter("yyyy-MM-dd'T'HHmm"))
                    .registerTypeAdapter(LocalDate.class, new DateTypeConverter())
                    .create();
            String campaignJson = "{\"identifier\":\"IRS_2018_S1\",\"title\":\"2019 IRS Season 1\",\"description\":\"This is the 2010 IRS Spray Campaign for Zambia for the first spray season dated 1 Jan 2019 - 31 Mar 2019.\",\"status\":\"In Progress\",\"executionPeriod\":{\"start\":\"2019-01-01\",\"end\":\"2019-03-31\"},\"authoredOn\":\"2018-10-01T0900\",\"lastModified\":\"2018-10-01T0900\",\"owner\":\"jdoe\",\"serverVersion\":0}";
            String campaign2Json = "{\"identifier\":\"IRS_2018_S2\",\"title\":\"2019 IRS Season 2\",\"description\":\"This is the 2010 IRS Spray Campaign for Zambia for the second spray season dated 1 Jan 2019 - 31 Mar 2019.\",\"status\":\"In Progress\",\"executionPeriod\":{\"start\":\"2019-01-01\",\"end\":\"2019-03-31\"},\"authoredOn\":\"2018-10-01T0900\",\"lastModified\":\"2018-10-01T0900\",\"owner\":\"jdoe\",\"serverVersion\":0}";

            Campaign campaign = gson.fromJson(campaignJson, Campaign.class);
            campaignRepository.addOrUpdate(campaign);

            Campaign campaign2 = gson.fromJson(campaign2Json, Campaign.class);
            campaignRepository.addOrUpdate(campaign2);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
