package org.smartregister.reveal.adapter;

import android.support.annotation.NonNull;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import org.smartregister.reveal.R;
import org.smartregister.reveal.model.Child;
import org.smartregister.view.ListContract;
import org.smartregister.view.adapter.ListableAdapter;
import org.smartregister.view.viewholder.ListableViewHolder;

import java.util.List;

public class ChildRegisterAdapter extends ListableAdapter<Child, ListableViewHolder<Child>> {

    public ChildRegisterAdapter(List<Child> items, ListContract.View<Child> view) {
        super(items, view);
    }

    @NonNull
    @Override
    public ListableViewHolder<Child> onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext()).inflate(R.layout.child_register_list_row, parent, false);
        return new ChildViewHolder(view);
    }

    public static class ChildViewHolder extends ListableViewHolder<Child> {

        private TextView tvName;
        private TextView tvDetails;
        private View currentView;
        private View linearLayoutAction;

        private ChildViewHolder(@NonNull View itemView) {
            super(itemView);
            currentView = itemView;
            tvName = itemView.findViewById(R.id.patient_name);
            tvDetails = itemView.findViewById(R.id.tvDetails);
            linearLayoutAction = itemView.findViewById(R.id.linearLayoutAction);
        }

        @Override
        public void bindView(Child child, ListContract.View<Child> view) {
            tvName.setText(child.getFullName());
            currentView.setOnClickListener(v -> view.onListItemClicked(child, v.getId()));
        }

        @Override
        public void resetView() {
            tvName.setText("");
            tvDetails.setText("");
            linearLayoutAction.setVisibility(View.GONE);
        }
    }
}
