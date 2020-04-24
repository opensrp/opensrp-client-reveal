package org.smartregister.reveal.fragment;

import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.text.Editable;
import android.text.TextWatcher;
import android.widget.TextView;
import android.widget.Toast;

import org.smartregister.reveal.R;
import org.smartregister.reveal.adapter.ChildRegisterAdapter;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.contract.ChildRegisterFragmentContract;
import org.smartregister.reveal.model.Child;
import org.smartregister.reveal.model.ChildModel;
import org.smartregister.reveal.presenter.ChildRegisterFragmentPresenter;
import org.smartregister.reveal.view.ChildRegisterActivity;
import org.smartregister.reveal.view.DrawerMenuView;
import org.smartregister.view.adapter.ListableAdapter;
import org.smartregister.view.fragment.BaseListFragment;
import org.smartregister.view.viewholder.ListableViewHolder;

import java.util.List;
import java.util.concurrent.Callable;

import timber.log.Timber;

public class ChildRegisterFragment extends BaseListFragment<Child> implements ChildRegisterFragmentContract.View, BaseDrawerContract.DrawerActivity {
    public static final String TAG = "ChildRegisterFragment";

    private BaseDrawerContract.View drawerView;

    @Override
    public void bindLayout() {
        super.bindLayout();
        drawerView = new DrawerMenuView(this);

        drawerView.initializeDrawerLayout();
        view.findViewById(R.id.drawerMenu).setOnClickListener(v -> drawerView.openDrawerLayout());

        drawerView.onResume();

        TextView mapText = view.findViewById(R.id.txt_map_label);
        mapText.setText("+ Add");
        mapText.setOnClickListener(v -> startChildRegistrationForm());

        TextView searchText = view.findViewById(R.id.edt_search);
        searchText.setHint("Search Students");
        searchText.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {
                loadPresenter().search(s.toString());
            }

            @Override
            public void afterTextChanged(Editable s) {

            }
        });

        view.findViewById(R.id.filter_text_view).setOnClickListener(v -> openFilterFragment());
    }

    @NonNull
    @Override
    protected Callable<List<Child>> onStartCallable(@Nullable Bundle bundle) {
        ChildModel model = presenter.getModel();
        return () -> model.searchChildren("", "");
    }

    @Override
    protected int getRootLayout() {
        return R.layout.fragment_child_register;
    }

    @Override
    protected int getRecyclerViewID() {
        return R.id.recycler_view;
    }

    @Override
    protected int getProgressBarID() {
        return R.id.client_list_progress;
    }

    @Override
    public void onListItemClicked(Child child, int layoutID) {
        Toast.makeText(getContext(), "You clicked on " + child.getFullName(), Toast.LENGTH_SHORT).show();
    }

    @Override
    public void onFetchError(Exception e) {
        Toast.makeText(getContext(), "An error occurred, handle it bro", Toast.LENGTH_SHORT).show();
        Timber.e(e);
    }

    @NonNull
    @Override
    public ListableAdapter<Child, ListableViewHolder<Child>> adapter() {
        return new ChildRegisterAdapter(list, this);
    }

    @NonNull
    @Override
    public ChildRegisterFragmentPresenter loadPresenter() {
        if (presenter == null) {
            presenter = new ChildRegisterFragmentPresenter()
                    .with(this)
                    .withModel(new ChildModel());
        }
        return (ChildRegisterFragmentPresenter) presenter;
    }

    @Override
    public void onDrawerClosed() {
        // to do -> re render the details
    }

    @Override
    public void openFilterFragment() {
        ChildRegisterActivity.startFragment(getActivity(), ChildFilterFragment.TAG, null, false);
    }

    @Override
    public void startChildRegistrationForm() {

    }
}
