package org.smartregister.reveal.util;

import android.app.Activity;
import android.content.res.Resources;
import android.text.TextUtils;
import android.util.Log;
import android.view.View;
import android.widget.TextView;

import org.smartregister.reveal.R;
import org.smartregister.reveal.model.CardDetails;
import org.smartregister.reveal.model.MosquitoHarvestCardDetails;
import org.smartregister.reveal.model.SprayCardDetails;
import org.smartregister.reveal.util.Constants.BusinessStatus;

import static org.smartregister.reveal.util.Constants.Intervention.LARVAL_DIPPING;
import static org.smartregister.reveal.util.Constants.Intervention.MOSQUITO_COLLECTION;

/**
 * Created by samuelgithengi on 3/22/19.
 */
public class CardDetailsUtil {

    private String TAG = CardDetailsUtil.class.getName();

    public static void formatCardDetails(CardDetails cardDetails) {
        if (cardDetails == null || cardDetails.getStatus() == null)
            return;
        // extract status color
        String status = cardDetails.getStatus();
        switch (status) {
            case BusinessStatus.NOT_SPRAYED:
            case BusinessStatus.INCOMPLETE:
            case BusinessStatus.IN_PROGRESS:
                cardDetails.setStatusColor(R.color.unsprayed);
                cardDetails.setStatusMessage(R.string.details_not_sprayed);
                break;
            case BusinessStatus.SPRAYED:
            case BusinessStatus.COMPLETE:
                cardDetails.setStatusColor(R.color.sprayed);
                cardDetails.setStatusMessage(R.string.details_sprayed);
                cardDetails.setReason(null);
                break;
            case BusinessStatus.NOT_SPRAYABLE:
            case BusinessStatus.NOT_ELIGIBLE:
                cardDetails.setStatusColor(R.color.unsprayable);
                cardDetails.setStatusMessage(R.string.details_not_sprayable);
                cardDetails.setReason(null);
                break;
            default:
                Log.w(CardDetailsUtil.class.getName(), "business status not defined :" + cardDetails.getStatus());
                break;
        }
    }

    public void populateSprayCardTextViews(SprayCardDetails sprayCardDetails, Activity activity) {
        try {
            TextView tvSprayStatus = activity.findViewById(R.id.spray_status);
            TextView tvPropertyType = activity.findViewById(R.id.property_type);
            TextView tvSprayDate = activity.findViewById(R.id.spray_date);
            TextView tvSprayOperator = activity.findViewById(R.id.user_id);
            TextView tvFamilyHead = activity.findViewById(R.id.family_head);
            TextView tvReason = activity.findViewById(R.id.reason);

            Integer color = sprayCardDetails.getStatusColor();
            tvSprayStatus.setTextColor(color == null ? activity.getResources().getColor(R.color.black) : activity.getResources().getColor(color));

            Integer status = sprayCardDetails.getStatusMessage();
            tvSprayStatus.setText(status == null ? "" : activity.getString(status));

            tvPropertyType.setText(sprayCardDetails.getPropertyType());
            tvSprayDate.setText(sprayCardDetails.getSprayDate());
            tvSprayOperator.setText(sprayCardDetails.getSprayOperator());
            tvFamilyHead.setText(sprayCardDetails.getFamilyHead());
            if (!TextUtils.isEmpty(sprayCardDetails.getReason())) {
                tvReason.setVisibility(View.VISIBLE);
                tvReason.setText(sprayCardDetails.getReason());
            } else {
                tvReason.setVisibility(View.GONE);
            }
        } catch (Resources.NotFoundException e) {
            Log.e(TAG, e.getStackTrace().toString());
        }
    }

    public void populateAndOpenMosquitoHarvestCard(MosquitoHarvestCardDetails mosquitoHarvestCardDetails, Activity activity) {
        String interventionType = mosquitoHarvestCardDetails.getInterventionType();
        String startDate = mosquitoHarvestCardDetails.getStartDate() != null ? mosquitoHarvestCardDetails.getStartDate() : "";
        String endDate = mosquitoHarvestCardDetails.getEndDate() != null ? mosquitoHarvestCardDetails.getEndDate() : "";
        if (MOSQUITO_COLLECTION.equals(interventionType)) {
            TextView tvMosquitoCollectionStatus = activity.findViewById(R.id.trap_collection_status);
            TextView tvMosquitoTrapSetDate = activity.findViewById(R.id.trap_set_date);
            TextView tvMosquitoTrapFollowUpDate = activity.findViewById(R.id.trap_follow_up_date);

            tvMosquitoCollectionStatus.setText(mosquitoHarvestCardDetails.getStatus());
            tvMosquitoTrapSetDate.setText(activity.getResources().getString(R.string.mosquito_collection_trap_set_date) + startDate);
            tvMosquitoTrapFollowUpDate.setText(activity.getResources().getString(R.string.mosquito_collection_trap_follow_up_date) + endDate);
            activity.findViewById(R.id.mosquito_collection_card_view).setVisibility(View.VISIBLE);
        } else if (LARVAL_DIPPING.equals(interventionType)) {
            TextView tvIdentifiedDate = activity.findViewById(R.id.larval_identified_date);
            TextView tvLarvicideDate = activity.findViewById(R.id.larvacide_date);

            tvIdentifiedDate.setText(activity.getResources().getString(R.string.larval_breeding_identified_date_test_text) + startDate);
            tvLarvicideDate.setText(activity.getResources().getString(R.string.larval_breeding_larvacide_date_test_text) + endDate);
            activity.findViewById(R.id.larval_breeding_card_view).setVisibility(View.VISIBLE);
        }
    }
}
