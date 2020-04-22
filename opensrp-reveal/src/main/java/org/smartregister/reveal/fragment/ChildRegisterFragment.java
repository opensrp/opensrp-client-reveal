package org.smartregister.reveal.fragment;

import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.widget.Toast;

import org.smartregister.reveal.R;
import org.smartregister.reveal.adapter.ChildRegisterAdapter;
import org.smartregister.reveal.model.Child;
import org.smartregister.reveal.model.ChildModel;
import org.smartregister.reveal.presenter.ChildRegisterFragmentPresenter;
import org.smartregister.view.ListContract;
import org.smartregister.view.adapter.ListableAdapter;
import org.smartregister.view.fragment.BaseListFragment;
import org.smartregister.view.viewholder.ListableViewHolder;

import java.util.List;
import java.util.concurrent.Callable;

import timber.log.Timber;

public class ChildRegisterFragment extends BaseListFragment<Child> {
    public static final String TAG = "ChildRegisterFragment";

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
    public ListContract.Presenter<Child> loadPresenter() {
        if (presenter == null) {
            presenter = new ChildRegisterFragmentPresenter()
                    .with(this)
                    .withModel(new ChildModel());
        }
        return presenter;
    }
}
