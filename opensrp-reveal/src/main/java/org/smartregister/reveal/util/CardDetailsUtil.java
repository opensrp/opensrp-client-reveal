package org.smartregister.reveal.util;

import android.app.Activity;
import android.content.Context;
import android.content.res.Resources;
import android.text.TextUtils;
import android.view.View;
import android.widget.Button;
import android.widget.ScrollView;
import android.widget.TextView;

import org.smartregister.AllConstants;
import org.smartregister.reveal.R;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.model.CardDetails;
import org.smartregister.reveal.model.FamilyCardDetails;
import org.smartregister.reveal.model.IRSVerificationCardDetails;
import org.smartregister.reveal.model.MosquitoHarvestCardDetails;
import org.smartregister.reveal.model.SprayCardDetails;
import org.smartregister.reveal.util.Constants.BusinessStatus;

import timber.log.Timber;

import static org.smartregister.reveal.util.Constants.BusinessStatus.COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.INCOMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.IN_PROGRESS;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_ELIGIBLE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_SPRAYABLE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_SPRAYED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_VISITED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.PARTIALLY_SPRAYED;
import static org.smartregister.reveal.util.Constants.BusinessStatus.SPRAYED;
import static org.smartregister.reveal.util.Constants.Intervention.LARVAL_DIPPING;
import static org.smartregister.reveal.util.Constants.Intervention.MOSQUITO_COLLECTION;
import static org.smartregister.reveal.util.Constants.Intervention.PAOT;

/**
 * Created by samuelgithengi on 3/22/19.
 */
public class CardDetailsUtil {

    public static void formatCardDetails(CardDetails cardDetails) {
        if (cardDetails == null || cardDetails.getStatus() == null)
            return;
        // extract status color
        String status = cardDetails.getStatus();
        switch (status) {
            case BusinessStatus.NOT_SPRAYED:
            case BusinessStatus.INCOMPLETE:
            case BusinessStatus.IN_PROGRESS:
            case BusinessStatus.NONE_RECEIVED:
                cardDetails.setStatusColor(R.color.unsprayed);
                cardDetails.setStatusMessage(R.string.details_not_sprayed);
                break;
            case BusinessStatus.SPRAYED:
            case BusinessStatus.COMPLETE:
            case BusinessStatus.FULLY_RECEIVED:
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
            case PARTIALLY_SPRAYED:
                cardDetails.setStatusColor(R.color.partially_sprayed);
                cardDetails.setStatusMessage(R.string.partially_sprayed);
                break;
            default:
                Timber.w("business status not defined :" + cardDetails.getStatus());
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
            Timber.e(e);
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
        } else if (PAOT.equals(interventionType)) {
            String lastUpdatedDate =  mosquitoHarvestCardDetails.getStartDate();

            TextView lastUpdatedDateView = activity.findViewById(R.id.paot_last_updated_date);
            lastUpdatedDateView.setText(activity.getResources().getString(R.string.paot_last_updated_date_test_text) + lastUpdatedDate);
            activity.findViewById(R.id.potential_area_of_transmission_card_view).setVisibility(View.VISIBLE);
        }
    }

    public void populateAndOpenIRSVerificationCard(IRSVerificationCardDetails cardDetails, Activity activity) {
        try {

            TextView tvIneligibleStructuresLabel = activity.findViewById(R.id.ineligible_structures_label);
            ScrollView svEligibleStructuresScrollView = activity.findViewById(R.id.eligible_structures_scrollview);

            if(cardDetails.getTrueStructure().equalsIgnoreCase(AllConstants.BOOLEAN_FALSE)) {

                tvIneligibleStructuresLabel.setText(activity.getResources().getString(R.string.not_true_structure));
                tvIneligibleStructuresLabel.setVisibility(View.VISIBLE);
                svEligibleStructuresScrollView.setVisibility(View.GONE);
            } else if(cardDetails.getEligStruc().equalsIgnoreCase(AllConstants.BOOLEAN_FALSE)) {

                tvIneligibleStructuresLabel.setText(activity.getResources().getString(R.string.structure_ineligible));
                tvIneligibleStructuresLabel.setVisibility(View.VISIBLE);
                svEligibleStructuresScrollView.setVisibility(View.GONE);
            } else {

                tvIneligibleStructuresLabel.setVisibility(View.GONE);
                svEligibleStructuresScrollView.setVisibility(View.VISIBLE);
                TextView tvReportedSprayLabel = activity.findViewById(R.id.reported_spray_label);
                TextView tvReportedSprayStatus = activity.findViewById(R.id.reported_spray_status);
                TextView tvChalkSprayLabel = activity.findViewById(R.id.chalk_spray_label);
                TextView tvChalkSprayStatus = activity.findViewById(R.id.chalk_spray_status);
                TextView tvStickerSprayLabel = activity.findViewById(R.id.sticker_spray_label);
                TextView tvStickerSprayStatus = activity.findViewById(R.id.sticker_spray_status);
                TextView tvCardSprayLabel = activity.findViewById(R.id.card_spray_label);
                TextView tvCardSprayStatus = activity.findViewById(R.id.card_spray_status);

                tvReportedSprayLabel.setText(activity.getResources().getString(R.string.reported_spray_status) + ":");
                tvReportedSprayStatus.setText(getTranslatedIRSVerificationStatus(cardDetails.getReportedSprayStatus()));

                tvChalkSprayLabel.setText(activity.getResources().getString(R.string.chalk_spray_status) + ":");
                tvChalkSprayStatus.setText(getTranslatedIRSVerificationStatus(cardDetails.getChalkSprayStatus()));

                tvStickerSprayLabel.setText(activity.getResources().getString(R.string.sticker_spray_status) + ":");
                tvStickerSprayStatus.setText(getTranslatedIRSVerificationStatus(cardDetails.getStickerSprayStatus()));

                tvCardSprayLabel.setText(activity.getResources().getString(R.string.card_spray_status) + ":");
                tvCardSprayStatus.setText(getTranslatedIRSVerificationStatus(cardDetails.getCardSprayStatus()));
            }

            activity.findViewById(R.id.irs_verification_card_view).setVisibility(View.VISIBLE);
        } catch (Resources.NotFoundException e) {
            Timber.e(e);
        }
    }

    public void populateFamilyCard(FamilyCardDetails familyCardDetails, Activity activity) {
        try {
            TextView tvSprayStatus = activity.findViewById(R.id.spray_status);
            TextView tvPropertyType = activity.findViewById(R.id.property_type);
            TextView tvSprayDate = activity.findViewById(R.id.spray_date);
            TextView tvSprayOperator = activity.findViewById(R.id.user_id);
            TextView tvFamilyHead = activity.findViewById(R.id.family_head);
            TextView tvReason = activity.findViewById(R.id.reason);
            Button changeSprayStatus = activity.findViewById(R.id.change_spray_status);
            Button registerFamily  =  activity.findViewById(R.id.register_family);

            Integer color = familyCardDetails.getStatusColor();
            tvSprayStatus.setTextColor(color == null ? activity.getResources().getColor(R.color.black) : activity.getResources().getColor(color));

            tvSprayStatus.setText(familyCardDetails.getStatus());

            tvSprayDate.setText(familyCardDetails.getDateCreated());
            tvSprayOperator.setText(familyCardDetails.getOwner());

            registerFamily.setVisibility(View.VISIBLE);
            changeSprayStatus.setVisibility(View.GONE);
            tvPropertyType.setVisibility(View.GONE);
            tvFamilyHead.setVisibility(View.GONE);
            tvReason.setVisibility(View.GONE);

        } catch (Resources.NotFoundException e) {
            Timber.e(e);
        }
    }

    /**
     * Takes in a business status and returns the translated value according to locale set.
     *
     * @param businessStatus Business status of the task attached to a structure
     * @return status Translated status according to locale set
     */
    public static String getTranslatedBusinessStatus(String businessStatus) {
        Context context = RevealApplication.getInstance().getApplicationContext();

        if (businessStatus == null)
            return context.getString(R.string.not_eligible);
        switch (businessStatus) {
            case NOT_VISITED:
                return context.getString(R.string.not_visited);
            case NOT_SPRAYED:
                return context.getString(R.string.not_sprayed);
            case SPRAYED:
                return context.getString(R.string.sprayed);
            case NOT_SPRAYABLE:
                return context.getString(R.string.not_sprayable);
            case COMPLETE:
                return context.getString(R.string.complete);
            case INCOMPLETE:
                return context.getString(R.string.incomplete);
            case NOT_ELIGIBLE:
                return context.getString(R.string.not_eligible);
            case IN_PROGRESS:
                return context.getString(R.string.in_progress);
            case PARTIALLY_SPRAYED:
                return context.getString(R.string.partially_sprayed);
            default:
                return businessStatus;
        }

    }

    /**
     * Takes in a IRS intervention status and returns the translated value .
     *
     * @param status Status of the IRS Verification type
     * @return status Translated status
     */
    public static String getTranslatedIRSVerificationStatus(String status) {
        Context context = RevealApplication.getInstance().getApplicationContext();

        if (status == null)
            return context.getString(R.string.not_sprayed);
        switch (status) {
            case Constants.IRSVerificationStatus.SPRAYED:
                return context.getString(R.string.sprayed);
            case Constants.IRSVerificationStatus.NOT_SPRAYED:
                return context.getString(R.string.not_sprayed);
            case Constants.IRSVerificationStatus.NOT_FOUND_OR_VISITED:
                return context.getString(R.string.structure_not_found_or_visited_during_campaign);
            case Constants.IRSVerificationStatus.OTHER:
                return context.getString(R.string.other);
            default:
                return status;
        }


    }


}
