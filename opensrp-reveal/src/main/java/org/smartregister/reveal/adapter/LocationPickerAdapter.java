package org.smartregister.reveal.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import org.smartregister.domain.Location;
import org.smartregister.reveal.R;
import org.smartregister.reveal.viewholder.LocationPickerViewHolder;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by Richard Kareko on 9/22/20.
 */

public class LocationPickerAdapter extends RecyclerView.Adapter<LocationPickerViewHolder> {

    private Context context;

    private View.OnClickListener locationPickerClickHandler;

    private List<Location> locations;

    public LocationPickerAdapter(Context context, View.OnClickListener offlineMapClickHandler) {
        this.context = context;
        this.locationPickerClickHandler = offlineMapClickHandler;
        this.locations = new ArrayList<>();
    }


    @NonNull
    @Override
    public LocationPickerViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(context).inflate(R.layout.offline_map_row, parent, false);
        return new LocationPickerViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull LocationPickerViewHolder viewHolder, int position) {
        Location location = locations.get(position);
        viewHolder.setLocationNameLabel(location.getProperties().getName());
        viewHolder.setItemViewListener(location, locationPickerClickHandler);
    }

    @Override
    public int getItemCount() {
        return locations.size();
    }

    public void setLocations(List<Location> locations) {
        this.locations = locations;
        notifyDataSetChanged();
    }
}
