package org.smartregister.reveal.fragment;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.util.Pair;
import androidx.recyclerview.widget.RecyclerView;

import com.mapbox.mapboxsdk.offline.OfflineRegion;
import com.mapbox.mapboxsdk.offline.OfflineRegionError;
import com.mapbox.mapboxsdk.offline.OfflineRegionStatus;

import org.jetbrains.annotations.NotNull;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.R;
import org.smartregister.reveal.adapter.DownloadedOfflineMapAdapter;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.DownloadedOfflineMapsContract;
import org.smartregister.reveal.contract.OfflineMapDownloadCallback;
import org.smartregister.reveal.model.OfflineMapModel;
import org.smartregister.reveal.presenter.DownloadedOfflineMapsPresenter;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import io.ona.kujaku.helpers.OfflineServiceHelper;

import static com.mapbox.mapboxsdk.offline.OfflineRegion.STATE_INACTIVE;
import static org.smartregister.reveal.util.Utils.showWhenTrue;

public class DownloadedOfflineMapsFragment extends BaseOfflineMapsFragment implements DownloadedOfflineMapsContract.View, View.OnClickListener, OfflineRegion.OfflineRegionObserver {

    private RecyclerView downloadedMapsRecyclerView;
    private Button btnDownloadMap;

    private DownloadedOfflineMapAdapter adapter;

    private DownloadedOfflineMapsPresenter presenter;

    private List<OfflineMapModel> downloadedOfflineMapModelList = new ArrayList<>();

    private List<OfflineMapModel> offlineMapsTodelete = new ArrayList<>();

    private OfflineMapDownloadCallback callback;

    public static DownloadedOfflineMapsFragment newInstance(Bundle bundle, Context context) {

        DownloadedOfflineMapsFragment fragment = new DownloadedOfflineMapsFragment();
        if (bundle != null) {
            fragment.setArguments(bundle);
        }
        fragment.setPresenter(new DownloadedOfflineMapsPresenter(fragment, context));
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (presenter == null) {
            presenter = new DownloadedOfflineMapsPresenter(this, getContext());
        }
    }

    @Override
    protected void downloadCompleted(String mapUniqueName) {
        for (OfflineMapModel offlineMapModel : downloadedOfflineMapModelList) {
            if (offlineMapModel.getDownloadAreaId().equals(mapUniqueName)) {
                offlineMapModel.setOfflineMapStatus(OfflineMapModel.OfflineMapStatus.DOWNLOADED);
                callback.onMapDownloaded(offlineMapModel);
                break;
            }
        }
    }

    @Override
    protected void downloadStarted(String mapUniqueName) {
        // Do nothing
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
        downloadedMapsRecyclerView = view.findViewById(R.id.offline_map_recyclerView);
        btnDownloadMap =  view.findViewById(R.id.download_map);
        btnDownloadMap.setVisibility(View.GONE);
        btnDownloadMap.setOnClickListener(v-> downloadMap(offlineMapsTodelete));
        Button btnDeleteMap = view.findViewById(R.id.delete_map);
        btnDeleteMap.setOnClickListener(v -> presenter.onDeleteDownloadMap(offlineMapsTodelete));
        btnDeleteMap.setVisibility(View.VISIBLE);
    }

    public void downloadMap(@NotNull List<OfflineMapModel> offlineMapsToDelete) {
        for(OfflineMapModel model: offlineMapsToDelete) {
            model.getOfflineRegion().getStatus(new OfflineRegion.OfflineRegionStatusCallback() {
                @Override
                public void onStatus(OfflineRegionStatus status) {
                    if(status.getDownloadState() == STATE_INACTIVE) {
                        Toast.makeText(getContext(), R.string.downloading, Toast.LENGTH_SHORT).show();
                        model.getOfflineRegion().setObserver(DownloadedOfflineMapsFragment.this);
                        model.getOfflineRegion().setDownloadState(OfflineRegion.STATE_ACTIVE);
                        adapter.notifyDataSetChanged();
                        btnDownloadMap.setVisibility(View.GONE);
                    }
                }
                @Override
                public void onError(String error) {
                    //Do nothing
                }
            });
        }
    }

    private void initializeAdapter() {
        adapter = new DownloadedOfflineMapAdapter(this.getContext(), this);
        downloadedMapsRecyclerView.setAdapter(adapter);
        if (downloadedOfflineMapModelList != null) {
            setDownloadedOfflineMapModelList(downloadedOfflineMapModelList);
        }
    }

    public void updateOfflineMapsTodelete(View view) {
        CheckBox checkBox = (CheckBox) view;
        OfflineMapModel offlineMapModel = (OfflineMapModel) view.getTag(R.id.offline_map_checkbox);

        if (checkBox.isChecked()) {
            offlineMapsTodelete.clear();
            offlineMapsTodelete.add(offlineMapModel);
        } else {
            offlineMapsTodelete.remove(offlineMapModel);
        }
        showWhenTrue(btnDownloadMap, offlineMapModel.isPending && checkBox.isChecked());
    }

    @Override
    public void setDownloadedOfflineMapModelList(List<OfflineMapModel> downloadedOfflineMapModelList) {
        if (downloadedOfflineMapModelList == null) {
            return;
        } else if (adapter == null) {
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

        btnDownloadMap.setVisibility(View.GONE);
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

        List<OfflineMapModel> toRemove = new ArrayList<>();
        for (OfflineMapModel offlineMapModel: downloadedOfflineMapModelList ) {
            if (offlineMapModel.getDownloadAreaId().equals(mapUniqueName)){
                toRemove.add(offlineMapModel);
                offlineMapModel.setOfflineMapStatus(OfflineMapModel.OfflineMapStatus.READY);
                callback.onOfflineMapDeleted(offlineMapModel);
            }
        }
        downloadedOfflineMapModelList.removeAll(toRemove);

        setDownloadedOfflineMapModelList(downloadedOfflineMapModelList);
    }

    @Override
    protected void downloadStopped(String mapUniqueName) {
        // Do nothing
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

    @Override
    public void onStatusChanged(OfflineRegionStatus status) {
        if (status.isComplete()) {
            callback.onMapDownloaded(null);
        } else {
            double percentage = status.getRequiredResourceCount() >= 0
                    ? (100.0 * status.getCompletedResourceCount() / status.getRequiredResourceCount()) :
                    0.0;
            displayToast(RevealApplication.getInstance().getResources().getString(R.string.map_download_progress, percentage));
        }
    }

    @Override
    public void onError(OfflineRegionError error) {
        //Do nothing
    }

    @Override
    public void mapboxTileCountLimitExceeded(long limit) {
        //Do nothing
    }
}
