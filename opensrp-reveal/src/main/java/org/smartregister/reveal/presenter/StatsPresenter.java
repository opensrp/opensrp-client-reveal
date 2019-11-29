package org.smartregister.reveal.presenter;

import org.smartregister.reveal.contract.StatsContract;
import org.smartregister.reveal.interactor.StatsInteractor;

public class StatsPresenter implements StatsContract.Presenter {
    private StatsContract.Interactor interactor;

    private StatsContract.View view;

    public StatsPresenter(StatsContract.View view) {
        this.view = view;
        interactor = new StatsInteractor();
    }
}
