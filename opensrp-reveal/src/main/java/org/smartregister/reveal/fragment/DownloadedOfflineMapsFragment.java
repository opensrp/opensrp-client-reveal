package org.smartregister.reveal.fragment;

import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.Toast;

import org.smartregister.reveal.R;
import org.smartregister.reveal.adapter.DownloadedOfflineMapAdapter;
import org.smartregister.reveal.contract.DownloadedOfflineMapsContract;
import org.smartregister.reveal.model.OfflineMapModel;
import org.smartregister.reveal.presenter.DownloadedOfflineMapsPresenter;

import java.util.ArrayList;
import java.util.List;

public class DownloadedOfflineMapsFragment extends Fragment implements DownloadedOfflineMapsContract.View {

    private RecyclerView downloadedMapsrecyclerView;
    private DownloadedOfflineMapAdapter adapter;
    private Button btnDeleteMap;

    private DownloadedOfflineMapsPresenter presenter;

    private List<OfflineMapModel> downloadedOfflineMapModelList = new ArrayList<>();

    private List<OfflineMapModel> offlineMapsTodelete = new ArrayList<>();

    public static DownloadedOfflineMapsFragment newInstance(Bundle bundle) {

        DownloadedOfflineMapsFragment fragment = new DownloadedOfflineMapsFragment();
        if (bundle != null) {
            fragment.setArguments(bundle);
        }
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        presenter = new DownloadedOfflineMapsPresenter(this);
    }

    @Override
    public void onResume() {
        super.onResume();
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_offline_map, container, false);
        setUpViews(view);
        initializeAdapter();
        return view;
    }

    private void setUpViews(View view) {
        downloadedMapsrecyclerView = view.findViewById(R.id.offline_map_recyclerView);

        btnDeleteMap = view.findViewById(R.id.download_map);
        btnDeleteMap.setText(getString(R.string.delete).toUpperCase());

        btnDeleteMap.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                presenter.onDeleteDownloadMap(offlineMapsTodelete);
            }
        });

    }

    private void initializeAdapter() {
        adapter = new DownloadedOfflineMapAdapter(this.getContext(), onClickListener);
        downloadedMapsrecyclerView.setAdapter(adapter);
        if (downloadedOfflineMapModelList != null) {
            setDownloadedOfflineMapModelList(downloadedOfflineMapModelList);
        }

    }

    private View.OnClickListener onClickListener = new View.OnClickListener(){
        @Override
        public void onClick(View view) {
            OfflineMapModel offlineMapModel = (OfflineMapModel) view.getTag(R.id.offline_map_checkbox);
            displayToast("Delete offline map for " + offlineMapModel.getDownloadAreaLabel());
            offlineMapsTodelete.add(offlineMapModel);
        }
    };

    @Override
    public void setDownloadedOfflineMapModelList(List<OfflineMapModel> downloadedOfflineMapModelList) {
        if (adapter == null) {
            this.downloadedOfflineMapModelList = downloadedOfflineMapModelList;
        } else {
            adapter.setOfflineMapModels(downloadedOfflineMapModelList);
            this.downloadedOfflineMapModelList = downloadedOfflineMapModelList;
        }
    }

    @Override
    public void displayToast(String message) {
        Toast.makeText(getContext(), message, Toast.LENGTH_LONG).show();
    }

    @Override
    public void displayError(int title, String message) {
        Toast.makeText(getContext(), message, Toast.LENGTH_LONG).show();
    }

    public void updateDownloadedMapsList(OfflineMapModel offlineMapModel){
        downloadedOfflineMapModelList.add(offlineMapModel);
        setDownloadedOfflineMapModelList(downloadedOfflineMapModelList);
    }

}
