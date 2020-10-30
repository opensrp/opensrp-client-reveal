package org.smartregister.reveal.adapter;

import android.content.Context;
import android.util.Pair;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.CheckBox;
import android.widget.TextView;
import android.widget.Toast;

import org.smartregister.reveal.R;
import org.smartregister.reveal.model.LocationModel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Created by Richard Kareko on 9/22/20.
 */

public class ExpandableListViewAdapter extends BaseExpandableListAdapter {

    private Context context;
    private List<Pair<String, String>> listGroup;
    private Map<String, List<LocationModel>> childLocationsMap;
    private List<String> selectedLocationIds = new ArrayList<>();
    private int checkedBoxesCount;
    private boolean[] checkedGroup;
    private int expandedGroupPosition = 0;

    public ExpandableListViewAdapter(Context context, List<Pair<String, String>> listGroup, Map<String,
            List<LocationModel>> childLocationsMap) {
        this.context = context;
        this.listGroup = listGroup;
        this.childLocationsMap = childLocationsMap;
        checkedBoxesCount = 0;
    }

    @Override
    public int getGroupCount() {
        return listGroup.size();
    }

    @Override
    public int getChildrenCount(int groupPosition) {
        return childLocationsMap.get(listGroup.get(groupPosition).first).size();
    }

    @Override
    public String getGroup(int groupPosition) {
        return listGroup.get(groupPosition).second;
    }

    @Override
    public LocationModel getChild(int groupPosition, int childPosition) {
        return childLocationsMap.get(listGroup.get(groupPosition).first).get(childPosition);
    }

    @Override
    public long getGroupId(int groupPosition) {
        return groupPosition;
    }

    @Override
    public long getChildId(int groupPosition, int childPosition) {
        return childPosition;
    }

    @Override
    public boolean hasStableIds() {
        return false;
    }

    @Override
    public View getGroupView(int groupPosition, boolean b, View _view, ViewGroup viewGroup) {
        String itemGroup = getGroup(groupPosition);
        GroupViewHolder groupViewHolder;
        View view = _view;
        if(view == null){
            LayoutInflater inflater = (LayoutInflater)context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
            view = inflater.inflate(R.layout.expanded_list_group, null);
            groupViewHolder = new GroupViewHolder();
            groupViewHolder.tvGroup = view.findViewById(R.id.tv_group);
            groupViewHolder.cbGroup = view.findViewById(R.id.cb_group);
            groupViewHolder.cbGroup.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    int pos = (int)view.getTag();
                    checkedGroup[pos] = !checkedGroup[pos];
                    for(LocationModel item : childLocationsMap.get(listGroup.get(pos).first)){
                        item.setChecked(checkedGroup[pos]);
                        if (checkedGroup[pos]) {
                            selectedLocationIds.add(item.getId());
                        } else {
                            selectedLocationIds.remove(item.getId());
                        }
                    }
                    notifyDataSetChanged();
                }
            });
            view.setTag(groupViewHolder);
        }else {
            groupViewHolder = (GroupViewHolder)view.getTag();
        }
        groupViewHolder.tvGroup.setText(String.format("%s (%d)", itemGroup, getChildrenCount(groupPosition)));
        groupViewHolder.cbGroup.setChecked(checkedGroup[groupPosition]);
        groupViewHolder.cbGroup.setTag(groupPosition);
        return view;
    }

    @Override
    public View getChildView(final int groupPosition, final int childPosition, boolean b, View _view, ViewGroup viewGroup) {
        expandedGroupPosition = groupPosition; // For some reason the groupPosition defaults to 0 when checkbox is clicked
                                                //this variable keeps track of the correct value
        LocationModel expandedListText = getChild(groupPosition,childPosition);
        ChildViewHolder childViewHolder;
        View view = _view;
        if(view == null){
            LayoutInflater inflater = (LayoutInflater)context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
            view = inflater.inflate(R.layout.expanded_list_item, null);
            childViewHolder = new ChildViewHolder();
            childViewHolder.tvChild = view.findViewById(R.id.tv_child);
            childViewHolder.cbChild = view.findViewById(R.id.cb_child);
            childViewHolder.cbChild.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View view) {
                    CheckBox cb = (CheckBox) view;
                    int cbPosition = (int) cb.getTag();
                    LocationModel selectedItem = childLocationsMap.get(listGroup.get(expandedGroupPosition).first).get(cbPosition);
                    selectedItem.setChecked(cb.isChecked());
                    if(cb.isChecked()){
                        checkedBoxesCount++;
                        selectedLocationIds.add(selectedItem.getId());
                        Toast.makeText(context,"Checked value is: " +
                                        selectedItem.getName(),
                                Toast.LENGTH_SHORT).show();
                    }else {
                        checkedBoxesCount--;
                        if(checkedBoxesCount == 0){
                            Toast.makeText(context,"nothing checked",Toast.LENGTH_SHORT).show();
                        }else {
                            Toast.makeText(context,"unchecked " + selectedItem.getName(),Toast.LENGTH_SHORT).show();
                            selectedLocationIds.remove(selectedItem.getId());
                        }
                    }
                    notifyDataSetChanged();
                }
            });
            view.setTag(childViewHolder);
        }else {
            childViewHolder = (ChildViewHolder)view.getTag();
        }
        childViewHolder.cbChild.setChecked(expandedListText.isChecked());
        childViewHolder.cbChild.setTag(childPosition);
        childViewHolder.tvChild.setText(expandedListText.getName());
        return view;
    }

    public void clearChecks() {
        for(int i=0; i<checkedGroup.length; i++) checkedGroup[i] = false;
        for(List<LocationModel> value : childLocationsMap.values()) {
            for (LocationModel sample : value) {
                sample.setChecked(false);
            }
        }
        checkedBoxesCount = 0;
        notifyDataSetChanged();
    }

    @Override
    public boolean isChildSelectable(int groupPosition, int childPosition) {
        return true;
    }

    private class GroupViewHolder {
        private CheckBox cbGroup;
        private TextView tvGroup;
    }

    private class ChildViewHolder {
        private CheckBox cbChild;
        private TextView tvChild;
    }

    public void setListGroup(List<Pair<String, String>> listGroup) {
        this.listGroup = listGroup;
    }

    public void setChildLocationsMap(Map<String, List<LocationModel>> childLocationsMap) {
        this.childLocationsMap = childLocationsMap;
    }

    public List<String> getSelectedLocationIds() {
        return selectedLocationIds;
    }

    public void initializeLocationList(List<Pair<String, String>> parentLocations, HashMap<String, List<LocationModel>> groupedLocations) {
        checkedGroup = new boolean[parentLocations.size()];
        setListGroup(parentLocations);
        setChildLocationsMap(groupedLocations);
        notifyDataSetChanged();
    }
}
