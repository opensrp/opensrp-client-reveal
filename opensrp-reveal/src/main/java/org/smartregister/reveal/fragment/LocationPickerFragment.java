package org.smartregister.reveal.fragment;

import android.content.Intent;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CheckBox;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.recyclerview.widget.RecyclerView;

import org.smartregister.domain.Location;
import org.smartregister.p2p.activity.P2pModeSelectActivity;
import org.smartregister.reveal.R;
import org.smartregister.reveal.adapter.LocationPickerAdapter;
import org.smartregister.reveal.contract.LocationPickerFragmentContract;
import org.smartregister.reveal.presenter.LocationPickerFragmentPresenter;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by Richard Kareko on 9/22/20.
 */

public class LocationPickerFragment extends Fragment implements LocationPickerFragmentContract.View, View.OnClickListener {

    private RecyclerView locationPickerRecyclerView;

    private LocationPickerAdapter adapter;

    private LocationPickerFragmentPresenter presenter;

    private List<Location> availableLocations = new ArrayList<>();

    private List<String> selectedLocationIds = new ArrayList<>();

    private Button btnP2PSync;

    public static LocationPickerFragment newInstance(Bundle bundle) {

        LocationPickerFragment fragment = new LocationPickerFragment();
        if (bundle != null) {
            fragment.setArguments(bundle);
        }
        fragment.setPresenter(new LocationPickerFragmentPresenter(fragment));

        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (presenter == null) {
            presenter = new LocationPickerFragmentPresenter(this);
        }
        btnP2PSync = null;
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_offline_map, container, false);
        setUpViews(view);
        initializeAdapter();
        presenter.fetchAvailableLocations(null);
        return view;
    }

    private void setUpViews(View view) {
        locationPickerRecyclerView = view.findViewById(R.id.offline_map_recyclerView);

        btnP2PSync = view.findViewById(R.id.download_map);
        btnP2PSync.setText(R.string.start_p2p_syc);
        btnP2PSync.setOnClickListener(this);

    }

    private void initializeAdapter() {
        adapter = new LocationPickerAdapter(this.getContext(), this);
        locationPickerRecyclerView.setAdapter(adapter);
        if (availableLocations != null) {
            setAvailableLocations(availableLocations);
        }

    }

    @Override
    public void disableCheckBox(String locationId) {

    }

    @Override
    public void enableCheckBox(String locationId) {

    }

    @Override
    public void setAvailableLocations(List<Location> locations) {
        if (availableLocations == null) {
            return;
        }
        if (adapter == null) {
            this.availableLocations = locations;
        } else {
            adapter.setLocations(locations);
            this.availableLocations = locations;
        }
    }

    public void updateSelectedLocations(View view) {
        CheckBox checkBox = (CheckBox) view;
        Location location = (Location) view.getTag(R.id.offline_map_checkbox);

        if (checkBox.isChecked()) {
            selectedLocationIds.add(location.getProperties().getName());
        } else {
            selectedLocationIds.remove(location.getProperties().getName());
        }
    }

    public void setPresenter(LocationPickerFragmentPresenter presenter) {
        this.presenter = presenter;
    }

    @Override
    public void onClick(View view) {
        switch (view.getId()) {
            case R.id.offline_map_checkbox:
                updateSelectedLocations(view);
                break;
            case R.id.download_map:
                initiateP2PSync();
                break;
            default:
                break;
        }
    }

    public void initiateP2PSync() {
        getContext().startActivity(new Intent(getContext(), P2pModeSelectActivity.class));
    }
}
