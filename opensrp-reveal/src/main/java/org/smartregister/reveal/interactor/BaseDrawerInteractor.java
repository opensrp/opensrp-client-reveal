package org.smartregister.reveal.interactor;

import org.smartregister.domain.Campaign;
import org.smartregister.repository.CampaignRepository;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.util.AppExecutors;

import java.util.List;

/**
 * Created by samuelgithengi on 3/21/19.
 */
public class BaseDrawerInteractor implements BaseDrawerContract.Interactor {

    private AppExecutors appExecutors;

    private BaseDrawerContract.Presenter presenter;

    private CampaignRepository campaignRepository;

    public BaseDrawerInteractor(BaseDrawerContract.Presenter presenter) {
        this.presenter = presenter;
        appExecutors = RevealApplication.getInstance().getAppExecutors();
        campaignRepository = RevealApplication.getInstance().getCampaignRepository();
    }

    @Override
    public void fetchCampaigns() {
        Runnable runnable = new Runnable() {
            @Override
            public void run() {
                List<Campaign> campaigns = campaignRepository.getAllCampaigns();
                appExecutors.mainThread().execute(new Runnable() {
                    @Override
                    public void run() {
                        presenter.onCampaignsFetched(campaigns);
                    }
                });

            }
        };

        appExecutors.diskIO().execute(runnable);
    }
}
