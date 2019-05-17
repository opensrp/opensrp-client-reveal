package org.smartregister.reveal.fragment;

import android.view.View;
import android.widget.EditText;

import org.smartregister.family.fragment.BaseFamilyRegisterFragment;
import org.smartregister.family.model.BaseFamilyRegisterFramentModel;
import org.smartregister.reveal.R;
import org.smartregister.reveal.presenter.FamilyRegisterFragmentPresenter;
import org.smartregister.view.activity.BaseRegisterActivity;

import java.util.HashMap;

/**
 * Created by samuelgithengi on 2/8/19.
 */
public class FamilyRegisterFragment extends BaseFamilyRegisterFragment {
    @Override
    protected void initializePresenter() {
        if (getActivity() == null) {
            return;
        }

        String viewConfigurationIdentifier = ((BaseRegisterActivity) getActivity()).getViewIdentifiers().get(0);
        presenter = new FamilyRegisterFragmentPresenter(this, new BaseFamilyRegisterFramentModel(), viewConfigurationIdentifier);
    }

    @Override
    public void setAdvancedSearchFormData(HashMap<String, String> hashMap) {//do nothing
    }

    @Override
    protected String getMainCondition() {
        return presenter().getMainCondition();
    }

    @Override
    protected String getDefaultSortQuery() {
        return presenter().getDefaultSortQuery();
    }


    @Override
    public void setupViews(View view) {
        super.setupViews(view);
        if (getSearchView() != null) {
            EditText searchView = view.findViewById(R.id.edt_search);
            searchView.setTextColor(getResources().getColor(R.color.text_black));
        }

        view.findViewById(org.smartregister.family.R.id.filter_sort_layout).setVisibility(View.GONE);
    }
}
