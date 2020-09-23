package org.smartregister.reveal.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.CheckBox;
import android.widget.TextView;
import android.widget.Toast;

import org.smartregister.reveal.R;
import org.smartregister.reveal.model.LocationModel;

import java.util.List;
import java.util.Map;

/**
 * Created by Richard Kareko on 9/22/20.
 */

public class ExpandableListViewAdapter extends BaseExpandableListAdapter {

    private Context context;
    private List<String> listGroup;
    private Map<String, List<LocationModel>> listChild;
    private int checkedBoxesCount;
    private boolean[] checkedGroup;
    private int expandedGroupPosition = 0;

    public ExpandableListViewAdapter(Context context, List<String> listGroup, Map<String,
            List<LocationModel>> listChild) {
        this.context = context;
        this.listGroup = listGroup;
        this.listChild = listChild;
        checkedBoxesCount = 0;
        //checkedGroup = new boolean[listGroup.size()];
        checkedGroup = new boolean[100];
    }

    @Override
    public int getGroupCount() {
        return listGroup.size();
    }

    @Override
    public int getChildrenCount(int groupPosition) {
        return listChild.get(listGroup.get(groupPosition)).size();
    }

    @Override
    public String getGroup(int groupPosition) {
        return listGroup.get(groupPosition);
    }

    @Override
    public LocationModel getChild(int groupPosition, int childPosition) {
        return listChild.get(listGroup.get(groupPosition)).get(childPosition);
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
    public View getGroupView(int groupPosition, boolean b, View view, ViewGroup viewGroup) {
        String itemGroup = getGroup(groupPosition);
        GroupViewHolder groupViewHolder;
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
                    for(LocationModel item : listChild.get(listGroup.get(pos))){
                        item.setChecked(checkedGroup[pos]);
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
    public View getChildView(final int groupPosition, final int childPosition, boolean b, View view, ViewGroup viewGroup) {
        expandedGroupPosition = groupPosition; // For some reason the groupPosition defaults to 0 when checkbox is clicked
                                                //this value keeps track of the correct value
        LocationModel expandedListText = getChild(groupPosition,childPosition);
        ChildViewHolder childViewHolder;
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
                    LocationModel selectedItem = listChild.get(listGroup.get(expandedGroupPosition)).get(childPosition);
                    selectedItem.setChecked(cb.isChecked());
                    if(cb.isChecked()){
                        checkedBoxesCount++;
                        Toast.makeText(context,"Checked value is: " +
                                        selectedItem.getName(),
                                Toast.LENGTH_SHORT).show();
                    }else {
                        checkedBoxesCount--;
                        if(checkedBoxesCount == 0){
                            Toast.makeText(context,"nothing checked",Toast.LENGTH_SHORT).show();
                        }else {
                            Toast.makeText(context,"unchecked",Toast.LENGTH_SHORT).show();
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
        childViewHolder.tvChild.setText(expandedListText.getName());
        return view;
    }

    public void clearChecks() {
        for(int i=0; i<checkedGroup.length; i++) checkedGroup[i] = false;
        for(List<LocationModel> value : listChild.values()) {
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
        CheckBox cbGroup;
        TextView tvGroup;
    }

    private class ChildViewHolder {
        CheckBox cbChild;
        TextView tvChild;
    }

    public void setListGroup(List<String> listGroup) {
        this.listGroup = listGroup;
    }

    public void setListChild(Map<String, List<LocationModel>> listChild) {
        this.listChild = listChild;
    }


}
