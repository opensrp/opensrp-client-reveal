package org.smartregister.reveal.fragment;

import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.content.ContextCompat;
import android.support.v4.util.Pair;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CheckBox;

import com.mapbox.mapboxsdk.offline.OfflineRegion;

import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.adapter.DownloadedOfflineMapAdapter;
import org.smartregister.reveal.contract.DownloadedOfflineMapsContract;
import org.smartregister.reveal.contract.OfflineMapDownloadCallback;
import org.smartregister.reveal.model.OfflineMapModel;
import org.smartregister.reveal.presenter.DownloadedOfflineMapsPresenter;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import io.ona.kujaku.helpers.OfflineServiceHelper;

public class DownloadedOfflineMapsFragment extends BaseOfflineMapsFragment implements DownloadedOfflineMapsContract.View {

    private RecyclerView downloadedMapsrecyclerView;

    private DownloadedOfflineMapAdapter adapter;

    private DownloadedOfflineMapsPresenter presenter;

    private List<OfflineMapModel> downloadedOfflineMapModelList = new ArrayList<>();

    private List<OfflineMapModel> offlineMapsTodelete = new ArrayList<>();

    private OfflineMapDownloadCallback callback;

    public static DownloadedOfflineMapsFragment newInstance(Bundle bundle) {

        DownloadedOfflineMapsFragment fragment = new DownloadedOfflineMapsFragment();
        if (bundle != null) {
            fragment.setArguments(bundle);
        }
        fragment.setPresenter(new DownloadedOfflineMapsPresenter(fragment));
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (presenter == null) {
            presenter = new DownloadedOfflineMapsPresenter(this);
        }
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

        Button btnDeleteMap = view.findViewById(R.id.download_map);
        btnDeleteMap.setText(getString(R.string.delete).toUpperCase());
        btnDeleteMap.setBackground(ContextCompat.getDrawable(getContext(), R.drawable.delete_map_bg));

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

            switch (view.getId()) {
                case R.id.offline_map_checkbox:
                    updateOfflineMapsTodelete(view);
                    break;
                default:
                    break;
            }
        }
    };

    private void updateOfflineMapsTodelete(View view) {
        CheckBox checkBox = (CheckBox) view;
        OfflineMapModel offlineMapModel = (OfflineMapModel) view.getTag(R.id.offline_map_checkbox);

        if (checkBox.isChecked()) {
            offlineMapsTodelete.add(offlineMapModel);
        } else {
            offlineMapsTodelete.remove(offlineMapModel);
        }
    }

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
    public void deleteDownloadedOfflineMaps() {
        if (offlineMapsTodelete == null || offlineMapsTodelete.isEmpty()){
            displayToast(getString(R.string.select_offline_map_to_delete));
            return;
        }

        for (OfflineMapModel offlineMapModel: offlineMapsTodelete) {
            OfflineServiceHelper.deleteOfflineMap(getActivity(),
                    offlineMapModel.getDownloadAreaId(),
                    BuildConfig.MAPBOX_SDK_ACCESS_TOKEN);
        }
    }

    public void updateDownloadedMapsList(OfflineMapModel offlineMapModel){
        downloadedOfflineMapModelList.add(offlineMapModel);
        setDownloadedOfflineMapModelList(downloadedOfflineMapModelList);
    }

    @Override
    protected void mapDeletedSuccessfully(String mapUniqueName) {
        if (adapter ==null) {
            return;
        }
        for (OfflineMapModel offlineMapModel: downloadedOfflineMapModelList ) {
            if (offlineMapModel.getDownloadAreaId().equals(mapUniqueName)){
                downloadedOfflineMapModelList.remove(offlineMapModel);
                offlineMapModel.setOfflineMapStatus(OfflineMapModel.OFFLINE_MAP_STATUS.READY);
                callback.onOfflineMapDeleted(offlineMapModel);
            }
        }

        setDownloadedOfflineMapModelList(downloadedOfflineMapModelList);
    }

    public void setOfflineMapDownloadCallback(OfflineMapDownloadCallback callBack) {
        this.callback = callBack;
    }

    public void setOfflineDownloadedMapNames (Pair<List<String>, Map<String, OfflineRegion>> offlineRegionInfo) {
        if (offlineRegionInfo == null || offlineRegionInfo.first == null) {
            return;
        }
        presenter.fetchOAsWithOfflineDownloads(offlineRegionInfo);
    }

    public void setPresenter(DownloadedOfflineMapsPresenter presenter) {
        this.presenter = presenter;
    }

}
