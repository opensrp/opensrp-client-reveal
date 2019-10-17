package org.smartregister.reveal.fragment;

import android.app.AlertDialog;
import android.app.ProgressDialog;
import android.location.Location;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.Toast;

import org.json.JSONObject;
import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.OtherFormsfragmentContract;
import org.smartregister.reveal.presenter.OtherFormsFragmentPresenter;
import org.smartregister.reveal.util.LocationUtils;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.reveal.view.OtherFormsActivity;

public class OtherFormsFragment extends Fragment implements OtherFormsfragmentContract.View {
    
    private OtherFormsFragmentPresenter presenter;

    private RevealJsonFormUtils jsonFormUtils;

    private ProgressDialog progressDialog;

    private LocationUtils locationUtils;

    public static OtherFormsFragment newInstance(Bundle bundle) {

        OtherFormsFragment fragment = new OtherFormsFragment();
        if (bundle != null) {
            fragment.setArguments(bundle);
        }
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        presenter = new OtherFormsFragmentPresenter(this);
        progressDialog = new ProgressDialog(getContext());
        progressDialog.setCancelable(false);

        locationUtils =  new LocationUtils(getContext());
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        View rootView = inflater.inflate(R.layout.fragment_other_forms, container, false);
        initializeViews(rootView);
        return rootView;
    }

    private void initializeViews(View view)
    {
       Button daily_summary = view.findViewById(R.id.other_daily_summary);
        daily_summary.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                presenter.showBasicForm(org.smartregister.reveal.util.Constants.JsonForm.DAILY_SUMMARY_ZAMBIA);
            }
        });

        Button team_leader_dos = view.findViewById(R.id.other_team_leader_dos);
        team_leader_dos.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                presenter.showBasicForm(org.smartregister.reveal.util.Constants.JsonForm.TEAM_LEADER_DOS_ZAMBIA);
            }
        });

        Button cb_spray_area = view.findViewById(R.id.other_cb_spray_area);
        cb_spray_area.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                presenter.showBasicForm(org.smartregister.reveal.util.Constants.JsonForm.CB_SPRAY_AREA_ZAMBIA);
            }
        });

        Button irs_sa_decision = view.findViewById(R.id.other_irs_sa_decision);
        irs_sa_decision.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                presenter.showBasicForm(org.smartregister.reveal.util.Constants.JsonForm.IRS_SA_DECISION_ZAMBIA);
            }
        });

        Button mobilization = view.findViewById(R.id.other_mobilization_form);
        mobilization.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                presenter.showBasicForm(org.smartregister.reveal.util.Constants.JsonForm.MOBILIZATION_FORM_ZAMBIA);
            }
        });

        Button irs_field_officer = view.findViewById(R.id.other_irs_field_officer);
        irs_field_officer.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                presenter.showBasicForm(org.smartregister.reveal.util.Constants.JsonForm.IRS_FIELD_OFFICER_ZAMBIA);
            }
        });

        Button verification_form = view.findViewById(R.id.other_verification_form);
        verification_form.setOnClickListener(new View.OnClickListener() {
            public void onClick(View v) {
                presenter.showBasicForm(org.smartregister.reveal.util.Constants.JsonForm.VERIFICATION_FORM_ZAMBIA);
            }
        });
    }

    @Override
    public void displayToast(String message) {
        Toast.makeText(getContext(), message, Toast.LENGTH_LONG).show();
    }

    @Override
    public RevealJsonFormUtils getJsonFormUtils() {
        return jsonFormUtils;
    }

    @Override
    public void startForm(JSONObject formName) {
        ((OtherFormsActivity) getActivity()).startFormActivity(formName);
    }

    @Override
    public void displayError(int title, int message) {
        new AlertDialog.Builder(getActivity()).setTitle(title).setMessage(message).create().show();
    }

    @Override
    public Location getUserCurrentLocation() {
        return locationUtils.getLastLocation();
    }

    @Override
    public void showProgressDialog(int title, int message) {
        if (progressDialog != null) {
            progressDialog.setTitle(title);
            progressDialog.setMessage(getString(message));
            progressDialog.show();
        }
    }

    @Override
    public void hideProgressDialog() {
        if (progressDialog != null) {
            progressDialog.dismiss();
        }
    }

    @Override
    public void requestUserLocation() {
    }

    public void setJsonFormUtils(RevealJsonFormUtils jsonFormUtils) {
        this.jsonFormUtils = jsonFormUtils;
    }

}
