package org.smartregister.reveal.util;

import org.smartregister.reveal.R;
import org.smartregister.reveal.model.CardDetails;
import org.smartregister.reveal.util.Constants.BusinessStatus;

/**
 * Created by samuelgithengi on 3/22/19.
 */
public class CardDetailsUtil {

    public static void formatCardDetails(CardDetails cardDetails) {
        // extract status color
        String sprayStatus = cardDetails.getStatus();
        if (BusinessStatus.NOT_SPRAYED.equals(sprayStatus)) {
            cardDetails.setStatusColor(R.color.unsprayed);
            cardDetails.setStatusMessage(R.string.details_not_sprayed);
        } else if (BusinessStatus.SPRAYED.equals(sprayStatus)) {
            cardDetails.setStatusColor(R.color.sprayed);
            cardDetails.setStatusMessage(R.string.details_sprayed);
            cardDetails.setReason(null);
        } else {
            cardDetails.setStatusColor(R.color.unsprayable);
            cardDetails.setStatusMessage(R.string.details_not_sprayable);
            cardDetails.setReason(null);
        }

    }

}
