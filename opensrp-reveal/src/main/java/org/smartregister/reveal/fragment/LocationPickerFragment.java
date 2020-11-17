package org.smartregister.reveal.fragment;

import android.content.Intent;
import android.os.Bundle;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.ExpandableListView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import org.smartregister.CoreLibrary;
import org.smartregister.domain.Location;
import org.smartregister.domain.jsonmapping.util.LocationTree;
import org.smartregister.p2p.activity.P2pModeSelectActivity;
import org.smartregister.reveal.R;
import org.smartregister.reveal.adapter.ExpandableListViewAdapter;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.contract.LocationPickerFragmentContract;
import org.smartregister.reveal.model.LocationModel;
import org.smartregister.reveal.presenter.LocationPickerFragmentPresenter;
import org.smartregister.util.AssetHandler;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import static org.smartregister.reveal.util.Constants.Tags.OPERATIONAL_AREA;
import static org.smartregister.reveal.util.Constants.Tags.ZONE;

/**
 * Created by Richard Kareko on 9/22/20.
 */

public class LocationPickerFragment extends Fragment implements LocationPickerFragmentContract.View, View.OnClickListener {

    private ExpandableListView mExpandableListView;

    private ExpandableListViewAdapter mExpandableListAdapter;

    private LocationPickerFragmentPresenter presenter;

    private List<Pair<String, String>> parentLocations;

    private List<String> selectedLocationIds;

    private HashMap<String, List<LocationModel>> groupedLocations;

    private int mLastExpandedPosition = -1;

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
        selectedLocationIds = new ArrayList<>();
        groupedLocations = new HashMap<>();
        parentLocations = new ArrayList<>();
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.fragment_location_picker, container, false);
        setUpViews(view);
        initializeAdapter();
        presenter.fetchAvailableLocations(null);
        return view;
    }

    private void setUpViews(View view) {
        mExpandableListView = view.findViewById(R.id.expandedListView);

        btnP2PSync = view.findViewById(R.id.download_map);
        btnP2PSync.setText(R.string.start_p2p_syc);
        btnP2PSync.setOnClickListener(this);

    }

    private void initializeAdapter() {
        mExpandableListAdapter = new ExpandableListViewAdapter(getContext(), parentLocations , groupedLocations);
        mExpandableListView.setAdapter(mExpandableListAdapter);

        mExpandableListView.setOnGroupExpandListener(new ExpandableListView.OnGroupExpandListener() {
            @Override
            public void onGroupExpand(int groupPosition) {
                if(mLastExpandedPosition != -1 && (mLastExpandedPosition != groupPosition)){
                    mExpandableListView.collapseGroup(mLastExpandedPosition);
                }
                mLastExpandedPosition = groupPosition;
            }
        });

    }

    @Override
    public void setAvailableLocations(List<Location> locations) {
        RevealApplication.getInstance().getAppExecutors().diskIO().execute(() -> {
            String anmLocationHierachy = CoreLibrary.getInstance().context().allSettings().fetchANMLocation();
            LocationTree locationTree = AssetHandler.jsonStringToJava(anmLocationHierachy, LocationTree.class);
            // extract parent location name from the location hierarchy and pupulate them in parentLocations list
            for (Location location : locations) {
                String locationTag = location.getLocationTags() != null && location.getLocationTags().iterator().hasNext()
                        ? location.getLocationTags().iterator().next().getName() : "";
                if (locationTag.equals(ZONE) && !groupedLocations.containsKey(location.getId())) {
                    groupedLocations.put(location.getId(), new ArrayList<>());
                } else {
                    if (locationTag.equals(OPERATIONAL_AREA) && !groupedLocations.containsKey(location.getProperties().getParentId())) {
                        groupedLocations.put(location.getProperties().getParentId(), new ArrayList<>());
                        org.smartregister.domain.jsonmapping.Location parentLocationName =  locationTree.findLocation(location.getProperties().getParentId());
                        parentLocations.add(Pair.create(location.getProperties().getParentId(), parentLocationName.getName()));
                    }
                    LocationModel locationModel = new LocationModel();
                    locationModel.setId(location.getId());
                    locationModel.setName(location.getProperties().getName());
                    if (groupedLocations.get(location.getProperties().getParentId()) != null) {
                        groupedLocations.get(location.getProperties().getParentId()).add(locationModel);
                    }
                }
            }
            RevealApplication.getInstance().getAppExecutors().mainThread().execute(() -> {
                mExpandableListAdapter.initializeLocationList(parentLocations,groupedLocations);
            });
        });

    }

    public void setPresenter(LocationPickerFragmentPresenter presenter) {
        this.presenter = presenter;
    }

    @Override
    public void onClick(View view) {
        switch (view.getId()) {
            case R.id.download_map:
                initiateP2PSync();
                break;
            default:
                break;
        }
    }

    public void initiateP2PSync() {
        this.selectedLocationIds = mExpandableListAdapter.getSelectedLocationIds();
        //set p2p options
        CoreLibrary.getInstance().getP2POptions().setLocationsFilter(selectedLocationIds.toArray(new String[this.selectedLocationIds.size()]));
        getContext().startActivity(new Intent(getContext(), P2pModeSelectActivity.class));
    }
}
