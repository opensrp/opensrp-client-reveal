package org.smartregister.reveal.fragment;

import android.app.ProgressDialog;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;

import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.StatsFragmentContract;
import org.smartregister.reveal.presenter.StatsFragmentPresenter;

public class StatsFragment extends Fragment implements StatsFragmentContract.View {
    private StatsFragmentPresenter presenter;

    private ProgressDialog progressDialog;

    public static StatsFragment newInstance(Bundle bundle) {

        StatsFragment fragment = new StatsFragment();
        if (bundle != null) {
            fragment.setArguments(bundle);
        }
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        presenter = new StatsFragmentPresenter(this);
        progressDialog = new ProgressDialog(getContext());
        progressDialog.setCancelable(false);
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View rootView = inflater.inflate(R.layout.fragment_stats, container, false);
        initializeViews(rootView);
        return rootView;
    }

    private void initializeViews(View view)
    {
        Button daily_summary = view.findViewById(R.id.refresh_button);
        daily_summary.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                //presenter.doSomething()
            }
        });
    }

}
