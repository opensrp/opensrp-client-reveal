package org.smartregister.reveal.util;

import android.util.Log;

import org.smartregister.reveal.R;
import org.smartregister.reveal.model.CardDetails;
import org.smartregister.reveal.util.Constants.BusinessStatus;

/**
 * Created by samuelgithengi on 3/22/19.
 */
public class CardDetailsUtil {

    public static void formatCardDetails(CardDetails cardDetails) {
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
        }
    }

}
