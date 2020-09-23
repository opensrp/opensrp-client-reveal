package org.smartregister.reveal.fragment;

import android.content.Intent;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.ExpandableListView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import com.google.common.collect.Lists;

import org.smartregister.domain.Location;
import org.smartregister.p2p.activity.P2pModeSelectActivity;
import org.smartregister.reveal.R;
import org.smartregister.reveal.adapter.ExpandableListViewAdapter;
import org.smartregister.reveal.contract.LocationPickerFragmentContract;
import org.smartregister.reveal.model.LocationModel;
import org.smartregister.reveal.presenter.LocationPickerFragmentPresenter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.smartregister.reveal.util.Constants.Tags.OPERATIONAL_AREA;
import static org.smartregister.reveal.util.Constants.Tags.ZONE;

/**
 * Created by Richard Kareko on 9/22/20.
 */

public class LocationPickerFragment extends Fragment implements LocationPickerFragmentContract.View, View.OnClickListener {

    private ExpandableListView mExpandableListView;

    private ExpandableListViewAdapter mExpandableListAdapter;

    private LocationPickerFragmentPresenter presenter;

    private List<Location> availableLocations = new ArrayList<>();

    private List<String> selectedLocationIds = new ArrayList<>();

    private HashMap<String, List<LocationModel>> groupedLocations = new HashMap<>();

    int mLastExpandedPosition = -1;

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
        View view = inflater.inflate(R.layout.fragment_location_picker, container, false);
        setUpViews(view);
        initializeAdapter();
        presenter.fetchAvailableLocations(null);
        return view;
    }

    private void setUpViews(View view) {
        mExpandableListView = view.findViewById(R.id.expandedListView);
        //clearChecks = (Button)findViewById(R.id.btnClearChecks);

        List<String> listTitle = genGroupList();
        mExpandableListAdapter = new ExpandableListViewAdapter(getContext(), Lists.newArrayList(groupedLocations.keySet()) , groupedLocations);
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

        btnP2PSync = view.findViewById(R.id.download_map);
        btnP2PSync.setText(R.string.start_p2p_syc);
        btnP2PSync.setOnClickListener(this);

    }

    private List<String> genGroupList(){
        List<String> listGroup = new ArrayList<>();
        for(int i=1; i<10; i++){
            listGroup.add("Group: " + i);
        }
        return listGroup;
    }

    private Map<String, List<LocationModel>> genChildList(List<String> header){
        Map<String, List<LocationModel>> listChild = new HashMap<>();
        for(int i=0; i<header.size(); i++){
            List<LocationModel> childLocations = new ArrayList<>();
            int a = (int)(Math.random() * 8);
            for(int j=0; j<a; j++){
                LocationModel locationModel = new LocationModel("Child " + (j + 1));
                childLocations.add(locationModel);
            }
            listChild.put(header.get(i), childLocations);
        }
        return  listChild;
    }

    private void initializeAdapter() {
        //adapter = new LocationPickerAdapter(this.getContext(), this);
        //locationPickerRecyclerView.setAdapter(adapter);
        if (availableLocations != null) {
            setAvailableLocations(availableLocations);
        }

    }

    @Override
    public void setAvailableLocations(List<Location> locations) {
        for (Location location: locations) {
            String locationTag = location.getLocationTags().iterator().next().getName();
            if (locationTag.equals(ZONE) && !groupedLocations.containsKey(location.getId())) {
                groupedLocations.put(location.getId(), new ArrayList<>());
            } else {
                if (locationTag.equals(OPERATIONAL_AREA) && !groupedLocations.containsKey(location.getProperties().getParentId())) {
                    groupedLocations.put(location.getProperties().getParentId(), new ArrayList<>());
                }
                LocationModel locationModel = new LocationModel();
                locationModel.setName(location.getProperties().getName());
                groupedLocations.get(location.getProperties().getParentId()).add(locationModel);

            }
        }
        mExpandableListAdapter.setListGroup(Lists.newArrayList(groupedLocations.keySet()));
        mExpandableListAdapter.setListChild(groupedLocations);
        mExpandableListAdapter.notifyDataSetChanged();
/*        if (availableLocations == null) {
            return;
        }
        if (adapter == null) {
            this.availableLocations = locations;
        } else {
            adapter.setLocations(locations);
            this.availableLocations = locations;
        }*/
    }

    public void updateSelectedLocations(View view) {
        CheckBox checkBox = (CheckBox) view;
        Location location = (Location) view.getTag(R.id.offline_map_checkbox);

        if (checkBox.isChecked()) {
            selectedLocationIds.add(location.getId());
        } else {
            selectedLocationIds.remove(location.getId());
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
