package org.smartregister.reveal.fragment;

import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.util.Consumer;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.RadioButton;
import android.widget.Toast;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.domain.Location;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.ChildFilterFragmentContract;
import org.smartregister.reveal.model.ChildFilterFragmentModel;
import org.smartregister.reveal.presenter.ChildFilterFragmentPresenter;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.Utils;
import org.smartregister.reveal.view.ChildRegisterActivity;
import org.smartregister.util.GenericInteractor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import timber.log.Timber;


public class ChildFilterFragment extends Fragment implements ChildFilterFragmentContract.View {
    public static final String TAG = "ChildFilterFragment";
    private ChildFilterFragmentContract.Presenter presenter;
    protected ProgressBar progressBar;
    protected AtomicInteger incompleteRequests = new AtomicInteger(0);
    private View view;
    private LinearLayout linearLayoutGrades;
    private LinearLayout linearLayoutAges;
    private RadioButton radioGradeName;
    private RadioButton radioGradeAge;
    private RadioButton radioAge;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        view = inflater.inflate(R.layout.child_register_filter_fragment, container, false);
        bindLayout();
        loadPresenter();
        reloadParameters();

        return view;
    }

    private AppCompatActivity getAppCompatActivity() {
        return (AppCompatActivity) getActivity();
    }

    @Override
    public void bindLayout() {
        // toolbar
        Toolbar toolbar = view.findViewById(R.id.filter_tasks_toolbar);
        toolbar.setTitle(R.string.filter);
        getAppCompatActivity().setSupportActionBar(toolbar);
        getAppCompatActivity().getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getAppCompatActivity().getSupportActionBar().setHomeAsUpIndicator(R.drawable.ic_action_close);
        toolbar.setNavigationOnClickListener(v -> getActivity().finish());

        Button btnApplyFilters = view.findViewById(R.id.apply_filters);
        btnApplyFilters.setOnClickListener(v -> executeFilter());

        radioGradeName = view.findViewById(R.id.radioGradeName);
        radioGradeAge = view.findViewById(R.id.radioGradeAge);
        radioAge = view.findViewById(R.id.radioAge);

        view.findViewById(R.id.clear_filters).setOnClickListener(v -> clearFilters());

        progressBar = view.findViewById(R.id.progress_bar);
        progressBar.setVisibility(View.GONE);

        linearLayoutGrades = view.findViewById(R.id.linearLayoutGrades);
        linearLayoutGrades.removeAllViews();

        linearLayoutAges = view.findViewById(R.id.linearLayoutAges);
    }

    @Override
    public ChildFilterFragmentContract.Presenter loadPresenter() {
        if (presenter == null) {
            presenter = new ChildFilterFragmentPresenter()
                    .usingView(this)
                    .usingInteractor(new GenericInteractor())
                    .usingModel(new ChildFilterFragmentModel());
        }
        return presenter;
    }

    @Override
    public void reloadParameters() {
        presenter.fetchUniqueGrades(getCurrentLocation());
    }

    @Override
    public HashMap<String, List<String>> getFilterValues() {
        HashMap<String, List<String>> result = new HashMap<>();
        // get sort
        List<String> sort = new ArrayList<>();
        if (radioGradeName.isChecked()) {
            sort.add(Constants.DatabaseKeys.GRADE);
            sort.add(Constants.DatabaseKeys.LAST_NAME);
        }

        if (radioGradeAge.isChecked()) {
            sort.add(Constants.DatabaseKeys.GRADE);
            sort.add(Constants.DatabaseKeys.DOB + " DESC ");
        }

        if (radioAge.isChecked())
            sort.add(Constants.DatabaseKeys.DOB + " DESC ");

        result.put(Constants.ChildFilter.SORT, sort);

        result.put(Constants.ChildFilter.FILTER_GRADE, getSelectedCheckBoxValues(linearLayoutGrades));
        result.put(Constants.ChildFilter.FILTER_AGE, presenter.getSelectedAges(getSelectedCheckBoxValues(linearLayoutAges), linearLayoutAges.getContext()));

        return result;
    }

    private void readLayoutCheckBoxes(LinearLayout rootLayout, Consumer<CheckBox> consumer) {
        int childCount = rootLayout.getChildCount();
        while (childCount > 0) {
            View view = rootLayout.getChildAt(childCount - 1);
            if (view instanceof CheckBox)
                consumer.accept(((CheckBox) view));

            childCount--;
        }
    }

    private void resetCheckBoxes(LinearLayout rootLayout) {
        readLayoutCheckBoxes(rootLayout, checkBox -> checkBox.setChecked(false));
    }

    private List<String> getSelectedCheckBoxValues(LinearLayout rootLayout) {
        List<String> selectedValues = new ArrayList<>();

        readLayoutCheckBoxes(rootLayout, checkBox -> {
            if (checkBox.isChecked())
                selectedValues.add(checkBox.getText().toString());
        });

        return selectedValues;
    }

    @Override
    public void clearFilters() {
        radioGradeName.setChecked(true);
        resetCheckBoxes(linearLayoutGrades);
        resetCheckBoxes(linearLayoutAges);
    }

    @Override
    public void onGradesFetched(List<String> grades) {
        linearLayoutGrades.removeAllViews();
        linearLayoutGrades.setVisibility(grades.size() > 0 ? View.VISIBLE : View.GONE);
        for (String grade : grades) {
            CheckBox checkBox = new CheckBox(getContext());
            checkBox.setText(StringUtils.isBlank(grade) ? "Unknown" : grade);
            linearLayoutGrades.addView(checkBox);
        }
    }

    @Override
    public void onError(Exception e) {
        Toast.makeText(getContext(), R.string.an_error_occured, Toast.LENGTH_SHORT).show();
        Timber.e(e);
    }

    @Override
    public void setLoadingState(boolean loadingState) {
        int result = loadingState ? incompleteRequests.incrementAndGet() : incompleteRequests.decrementAndGet();
        progressBar.setVisibility(result > 0 ? View.VISIBLE : View.INVISIBLE);
    }

    @Override
    public void executeFilter() {
        Bundle bundle = new Bundle();
        bundle.putSerializable(Constants.ChildFilter.FILTER_PAYLOAD, getFilterValues());
        ChildRegisterActivity.startFragment(getActivity(), ChildRegisterFragment.TAG, bundle, true);
        getActivity().finish();
    }

    @Override
    public String getCurrentLocation() {
        Location location = Utils.getStructureByName(PreferencesUtil.getInstance().getCurrentStructure());
        return location == null ? null : location.getId();
    }
}
