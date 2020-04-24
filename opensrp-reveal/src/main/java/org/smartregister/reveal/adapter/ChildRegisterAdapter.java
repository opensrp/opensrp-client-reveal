package org.smartregister.reveal.adapter;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import org.smartregister.reveal.R;
import org.smartregister.reveal.model.Child;
import org.smartregister.reveal.viewholder.GroupedListableViewHolder;
import org.smartregister.view.ListContract;

import java.util.List;

public class ChildRegisterAdapter extends GroupedListableAdapter<Child, GroupedListableViewHolder<Child>> {

    public ChildRegisterAdapter(List<Child> items, ListContract.View<Child> view) {
        super(items, view);
    }

    @Override
    public void reloadData(@Nullable List<Child> items) {
        super.reloadData(items);
    }

    @NonNull
    @Override
    public GroupedListableViewHolder<Child> onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.child_register_list_row, parent, false);
        return new ChildViewHolder(view);
    }

    public static class ChildViewHolder extends GroupedListableViewHolder<Child> {

        private TextView tvName;
        private TextView tvDetails;
        private View currentView;
        private View linearLayoutAction;
        private TextView tvHeader;

        private ChildViewHolder(@NonNull View itemView) {
            super(itemView);
            currentView = itemView;
            tvHeader = itemView.findViewById(R.id.tvHeader);
            tvName = itemView.findViewById(R.id.patient_name);
            tvDetails = itemView.findViewById(R.id.tvDetails);
            linearLayoutAction = itemView.findViewById(R.id.linearLayoutAction);
        }

        @Override
        public void bindView(Child child, ListContract.View<Child> view) {
            tvName.setText(child.getFullName());
            String details = "#" + child.getUniqueID() + " \u00B7 " + child.getGender() + " \u00B7 Age " + child.getAge();
            tvDetails.setText(details);
            currentView.setOnClickListener(v -> view.onListItemClicked(child, v.getId()));
        }

        @Override
        public void resetView() {
            tvHeader.setText("");
            tvHeader.setVisibility(View.GONE);

            tvName.setText("");
            tvDetails.setText("");
            linearLayoutAction.setVisibility(View.GONE);
        }

        @Override
        public void bindHeader(Child currentObject, @Nullable Child previousObject, ListContract.View<Child> view) {
            if (previousObject == null || !currentObject.getGrade().equals(previousObject.getGrade())) {
                tvHeader.setVisibility(View.VISIBLE);
                tvHeader.setText(currentObject.getGrade());
            }
        }
    }
}
