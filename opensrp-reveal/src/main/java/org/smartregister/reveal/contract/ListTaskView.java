package org.smartregister.reveal.contract;

import android.content.Context;
import android.widget.TextView;

import org.smartregister.reveal.R;

/**
 * Created by samuelgithengi on 11/27/18.
 */
public interface ListTaskView {

    Context getContext();

    void setCampaign(String campaign);

    void setOperationalArea(String operationalArea);

    void setDistrict(String district);

    void setFacility(String facility);

    void setOperator();
}
