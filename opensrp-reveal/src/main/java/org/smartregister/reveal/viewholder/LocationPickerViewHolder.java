package org.smartregister.reveal.viewholder;

import android.view.View;
import android.widget.CheckBox;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import org.smartregister.domain.Location;
import org.smartregister.reveal.R;

/**
 * Created by Richard Kareko on 9/22/20.
 */

public class LocationPickerViewHolder extends RecyclerView.ViewHolder {

    protected CheckBox locationCheckBox;

    protected TextView tvLocationNameLabel;

    public LocationPickerViewHolder(@NonNull View itemView) {
        super(itemView);
        locationCheckBox = itemView.findViewById(R.id.offline_map_checkbox);
        tvLocationNameLabel = itemView.findViewById(R.id.offline_map_label);
    }

    public void setLocationNameLabel(String locationName) {
        this.tvLocationNameLabel.setText(locationName);
    }


    public void setItemViewListener(Location location, View.OnClickListener onClickListener) {
        locationCheckBox.setOnClickListener(onClickListener);
        locationCheckBox.setTag(R.id.offline_map_checkbox, location);
    }

    public void enableCheckBox(boolean enable) {
        locationCheckBox.setEnabled(enable);
    }

    public void checkCheckBox(boolean check) {
        locationCheckBox.setChecked(check);
    }
}
