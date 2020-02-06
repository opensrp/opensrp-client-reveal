package org.smartregister.reveal.fragment;

import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CheckBox;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.mapbox.geojson.Feature;

import org.joda.time.DateTime;
import org.smartregister.domain.Location;
import org.smartregister.domain.LocationProperty;
import org.smartregister.reveal.R;
import org.smartregister.reveal.adapter.AvailableOfflineMapAdapter;
import org.smartregister.reveal.contract.AvailableOfflineMapsContract;
import org.smartregister.reveal.contract.OfflineMapDownloadCallback;
import org.smartregister.reveal.model.OfflineMapModel;
import org.smartregister.reveal.presenter.AvailableOfflineMapsPresenter;
import org.smartregister.reveal.task.JavaHttpServerTask;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.OfflineMapHelper;
import org.smartregister.util.DateTimeTypeConverter;
import org.smartregister.util.PropertiesConverter;

import java.util.ArrayList;
import java.util.List;

public class AvailableOfflineMapsFragment extends BaseOfflineMapsFragment implements AvailableOfflineMapsContract.View {

    private RecyclerView offlineMapRecyclerView;

    private AvailableOfflineMapAdapter adapter;

    private AvailableOfflineMapsPresenter presenter;

    private List<OfflineMapModel> offlineMapModelList = new ArrayList<>();

    private List<Location> operationalAreasToDownload = new ArrayList<>();

    private OfflineMapDownloadCallback callback;

    private Button btnDownloadMap;


    public static AvailableOfflineMapsFragment newInstance(Bundle bundle) {

        AvailableOfflineMapsFragment fragment = new AvailableOfflineMapsFragment();
        if (bundle != null) {
            fragment.setArguments(bundle);
        }

        fragment.setPresenter(new AvailableOfflineMapsPresenter(fragment));
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (presenter == null) {
            presenter = new AvailableOfflineMapsPresenter(this);
        }
        btnDownloadMap = null;

        new JavaHttpServerTask(getContext()).execute();


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
        offlineMapRecyclerView = view.findViewById(R.id.offline_map_recyclerView);

        btnDownloadMap = view.findViewById(R.id.download_map);

        btnDownloadMap.setOnClickListener(onClickListener);

    }

    private void initializeAdapter() {
        adapter = new AvailableOfflineMapAdapter(this.getContext(), onClickListener);
        offlineMapRecyclerView.setAdapter(adapter);
        if (offlineMapModelList != null) {
            setOfflineMapModelList(offlineMapModelList);
        }

    }

    private View.OnClickListener onClickListener = new View.OnClickListener(){
        @Override
        public void onClick(View view) {
            switch (view.getId()) {
                case R.id.offline_map_checkbox:
                    updateOperationalAreasToDownload(view);
                    break;
                case R.id.download_map:
                    initiateMapDownload();
                    break;
                default:
                        break;
            }

        }
    };

    @Override
    public void setOfflineMapModelList(List<OfflineMapModel> offlineMapModelList) {
        if (offlineMapModelList == null) {
            return;
        }
        if (adapter == null) {
            this.offlineMapModelList = offlineMapModelList;
        } else {
            adapter.setOfflineMapModels(offlineMapModelList);
            this.offlineMapModelList = offlineMapModelList;
        }
    }

    public void updateOperationalAreasToDownload(View view) {
        CheckBox checkBox = (CheckBox) view;
        OfflineMapModel offlineMapModel = (OfflineMapModel) view.getTag(R.id.offline_map_checkbox);

        if (checkBox.isChecked()) {
            operationalAreasToDownload.add(offlineMapModel.getLocation());
        } else {
            operationalAreasToDownload.remove(offlineMapModel.getLocation());
        }
    }

    @Override
    public void disableCheckBox(String operationalAreaId) {
        if (adapter ==null) {
            return;
        }
        for (OfflineMapModel offlineMapModel: offlineMapModelList ) {
            if (offlineMapModel.getDownloadAreaId().equals(operationalAreaId)){
                if (offlineMapModel.getOfflineMapStatus() == OfflineMapModel.OfflineMapStatus.DOWNLOAD_STARTED) {
                    return;
                }
                offlineMapModel.setOfflineMapStatus(OfflineMapModel.OfflineMapStatus.DOWNLOAD_STARTED);
            }
        }

        setOfflineMapModelList(offlineMapModelList);
    }

    @Override
    public void enableCheckBox(String operationalAreaId) {
        if (adapter ==null) {
            return;
        }
        for (OfflineMapModel offlineMapModel: offlineMapModelList ) {
            if (offlineMapModel.getDownloadAreaId().equals(operationalAreaId)){
                if (offlineMapModel.getOfflineMapStatus() == OfflineMapModel.OfflineMapStatus.DOWNLOADED) {
                    return;
                }
                offlineMapModel.setOfflineMapStatus(OfflineMapModel.OfflineMapStatus.READY);
            }
        }

        setOfflineMapModelList(offlineMapModelList);
    }

    @Override
    public void moveDownloadedOAToDownloadedList(String operationalAreaId) {
        for (OfflineMapModel offlineMapModel : offlineMapModelList) {
            if (offlineMapModel.getDownloadAreaId().equals(operationalAreaId)) {
                offlineMapModel.setOfflineMapStatus(OfflineMapModel.OfflineMapStatus.DOWNLOADED);
                callback.onMapDownloaded(offlineMapModel);
                offlineMapModelList.remove(offlineMapModel);
                setOfflineMapModelList(offlineMapModelList);
                return;
            }
        }
    }

    @Override
    public void removeOperationalAreaToDownload(String operationalAreaId) {
        for (Location location: this.operationalAreasToDownload ) {
            if (location.getId().equals(operationalAreaId)) {
                this.operationalAreasToDownload.remove(location);
                return;
            }
        }

    }

    public void initiateMapDownload() {
        Gson gson = new GsonBuilder().setDateFormat(Constants.DateFormat.EVENT_DATE_FORMAT_Z)
                .registerTypeAdapter(DateTime.class, new DateTimeTypeConverter())
                .registerTypeAdapter(LocationProperty.class, new PropertiesConverter()).create();

        if (this.operationalAreasToDownload == null || this.operationalAreasToDownload.isEmpty()) {
            displayToast(getString(R.string.select_offline_map_to_download));
            return;
        }

        for (Location location: this.operationalAreasToDownload ) {
            Feature operationalAreaFeature = Feature.fromJson(gson.toJson(location));
            String mapName = location.getId();
            currentMapDownload = mapName;
            OfflineMapHelper.downloadMap(operationalAreaFeature, mapName, getActivity());
        }
    }

    public void setOfflineMapDownloadCallback(OfflineMapDownloadCallback callBack) {
        this.callback = callBack;
    }

    @Override
    protected void downloadCompleted(String mapUniqueName) {
        presenter.onDownloadComplete(mapUniqueName);
    }

    @Override
    protected void downloadStarted(String mapUniqueName) {
        presenter.onDownloadStarted(mapUniqueName);
    }

    @Override
    protected void downloadStopped(String mapUniqueName) {
        presenter.onDownloadStopped(mapUniqueName);
    }

    public void updateOperationalAreasToDownload(OfflineMapModel offlineMapModel){
        offlineMapModelList.add(offlineMapModel);
        setOfflineMapModelList(offlineMapModelList);
    }

    public void setOfflineDownloadedMapNames (List<String> offlineRegionNames) {
        presenter.fetchAvailableOAsForMapDownLoad(offlineRegionNames);
    }

    public void setPresenter(AvailableOfflineMapsPresenter presenter) {
        this.presenter = presenter;
    }

}
