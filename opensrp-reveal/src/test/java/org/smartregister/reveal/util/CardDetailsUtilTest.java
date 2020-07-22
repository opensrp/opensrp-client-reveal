package org.smartregister.reveal.util;

import android.app.Activity;
import android.content.res.Resources;
import androidx.cardview.widget.CardView;
import android.view.View;
import android.widget.Button;
import android.widget.ScrollView;
import android.widget.TextView;

import org.junit.Rule;
import org.junit.Test;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.model.CardDetails;
import org.smartregister.reveal.model.FamilyCardDetails;
import org.smartregister.reveal.model.IRSVerificationCardDetails;
import org.smartregister.reveal.model.MosquitoHarvestCardDetails;
import org.smartregister.reveal.model.SprayCardDetails;

import static org.junit.Assert.assertEquals;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.smartregister.AllConstants.BOOLEAN_FALSE;
import static org.smartregister.AllConstants.BOOLEAN_TRUE;
import static org.smartregister.reveal.util.CardDetailsUtil.formatCardDetails;
import static org.smartregister.reveal.util.CardDetailsUtil.getTranslatedBusinessStatus;
import static org.smartregister.reveal.util.CardDetailsUtil.getTranslatedIRSVerificationStatus;
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
 * Created by Vincent Karuri on 25/04/2019
 */
public class CardDetailsUtilTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Test
    public void testFormatCardDetailsNotSprayedIncompleteInProgressShouldSetCorrectStatusAndColor() {
        CardDetails cardDetails = new CardDetails(INCOMPLETE);
        formatCardDetails(cardDetails);
        assertEquals(cardDetails.getStatusColor().intValue(), R.color.unsprayed);
        assertEquals(cardDetails.getStatusMessage(), R.string.details_not_sprayed);

        cardDetails = new CardDetails(IN_PROGRESS);
        formatCardDetails(cardDetails);
        assertEquals(cardDetails.getStatusColor().intValue(), R.color.unsprayed);
        assertEquals(cardDetails.getStatusMessage(), R.string.details_not_sprayed);

        cardDetails = new CardDetails(NOT_SPRAYED);
        formatCardDetails(cardDetails);
        assertEquals(cardDetails.getStatusColor().intValue(), R.color.unsprayed);
        assertEquals(cardDetails.getStatusMessage(), R.string.details_not_sprayed);
    }

    @Test
    public void testFormatCardDetailsSprayedCompleteSetCorrectStatusAndColor() {
        CardDetails cardDetails = new CardDetails(COMPLETE);
        formatCardDetails(cardDetails);
        assertEquals(cardDetails.getStatusColor().intValue(), R.color.sprayed);
        assertEquals(cardDetails.getStatusMessage(), R.string.details_sprayed);

        cardDetails = new CardDetails(SPRAYED);
        formatCardDetails(cardDetails);
        assertEquals(cardDetails.getStatusColor().intValue(), R.color.sprayed);
        assertEquals(cardDetails.getStatusMessage(), R.string.details_sprayed);
    }

    @Test
    public void testFormatCardDetailsNotSprayableNotEligibleShouldSetCorrectStatusAndColor() {
        CardDetails cardDetails = new CardDetails(NOT_SPRAYABLE);
        formatCardDetails(cardDetails);
        assertEquals(cardDetails.getStatusColor().intValue(), R.color.unsprayable);
        assertEquals(cardDetails.getStatusMessage(), R.string.details_not_sprayable);

        cardDetails = new CardDetails(NOT_ELIGIBLE);
        formatCardDetails(cardDetails);
        assertEquals(cardDetails.getStatusColor().intValue(), R.color.unsprayable);
        assertEquals(cardDetails.getStatusMessage(), R.string.details_not_sprayable);
    }

    @Test
    public void testFormatCardDetailsPartiallySprayedShouldSetCorrectStatusAndColor() {
        CardDetails cardDetails = new CardDetails(PARTIALLY_SPRAYED);
        formatCardDetails(cardDetails);
        assertEquals(cardDetails.getStatusColor().intValue(), R.color.sprayed);
        assertEquals(cardDetails.getStatusMessage(), R.string.details_sprayed);
    }

    @Test
    public void testCardDetailsSetsDefaultStatusColorForNullBusinessStatus() {
        CardDetails cardDetails = new CardDetails(null);
        assertEquals(cardDetails.getStatusColor().intValue(), R.color.task_not_done);
        formatCardDetails(cardDetails);
        assertEquals(cardDetails.getStatusColor().intValue(), R.color.task_not_done);
    }

    @Test
    public void testPopulateSprayCardTextViewsShouldPopulateCorrectValues() {
        final String PROPERTY_TYPE = "Residential";
        final String SPRAY_DATE = "today";
        final String SPRAY_OPERATOR = "operator";
        final String FAMILY_HEAD = "head";
        final String REASON = "reason";
        final String STATUS = "status";
        final int STATUS_COLOR = 0;

        Activity activity = mock(Activity.class);
        Resources resources = mock(Resources.class);

        TextView tvSprayStatus = new TextView(RuntimeEnvironment.application);
        TextView tvPropertyType = new TextView(RuntimeEnvironment.application);
        TextView tvSprayDate = new TextView(RuntimeEnvironment.application);
        TextView tvSprayOperator = new TextView(RuntimeEnvironment.application);
        TextView tvFamilyHead = new TextView(RuntimeEnvironment.application);
        TextView tvReason = new TextView(RuntimeEnvironment.application);

        doReturn(resources).when(activity).getResources();
        doReturn(STATUS_COLOR).when(resources).getColor(anyInt());

        doReturn(tvSprayStatus).when(activity).findViewById(eq(R.id.spray_status));
        doReturn(tvPropertyType).when(activity).findViewById(eq(R.id.property_type));
        doReturn(tvSprayDate).when(activity).findViewById(eq(R.id.spray_date));
        doReturn(tvSprayOperator).when(activity).findViewById(eq(R.id.user_id));
        doReturn(tvFamilyHead).when(activity).findViewById(eq(R.id.family_head));
        doReturn(tvReason).when(activity).findViewById(eq(R.id.reason));
        doReturn(STATUS).when(activity).getString(anyInt());

        SprayCardDetails sprayCardDetails = new SprayCardDetails("", PROPERTY_TYPE, SPRAY_DATE, SPRAY_OPERATOR, FAMILY_HEAD, REASON);
        sprayCardDetails.setStatusColor(2);
        new CardDetailsUtil().populateSprayCardTextViews(sprayCardDetails, activity);

        assertEquals(tvSprayStatus.getText(), STATUS);
        assertEquals(tvPropertyType.getText(), PROPERTY_TYPE);
        assertEquals(tvSprayDate.getText(), SPRAY_DATE);
        assertEquals(tvSprayOperator.getText(), SPRAY_OPERATOR);
        assertEquals(tvFamilyHead.getText(), FAMILY_HEAD);
        assertEquals(tvReason.getText(), REASON);
        assertEquals(tvSprayStatus.getCurrentTextColor(), STATUS_COLOR);
    }

    @Test
    public void testPopulateAndOpenMosquitoHarvestCardShouldPopulateCorrectValuesAndOpenCardView() {
        final String TEST = "test ";
        final String START_DATE = "yesterday";
        final String END_DATE = "today";
        final String STATUS = "status";

        MosquitoHarvestCardDetails mosquitoHarvestCardDetails = new MosquitoHarvestCardDetails(TEST, START_DATE, END_DATE, MOSQUITO_COLLECTION);
        mosquitoHarvestCardDetails.setStatus(STATUS);

        Activity activity = mock(Activity.class);
        Resources resources = mock(Resources.class);

        doReturn(resources).when(activity).getResources();
        doReturn(TEST).when(resources).getString(anyInt());

        TextView tvMosquitoCollectionStatus = new TextView(RuntimeEnvironment.application);
        TextView tvMosquitoTrapSetDate = new TextView(RuntimeEnvironment.application);
        TextView tvMosquitoTrapFollowUpDate = new TextView(RuntimeEnvironment.application);
        TextView tvIdentifiedDate= new TextView(RuntimeEnvironment.application);
        TextView tvLarvicideDate = new TextView(RuntimeEnvironment.application);
        TextView tvLastUpdatedDateView = new TextView(RuntimeEnvironment.application);

        CardView mosquitoCollectionCardView = new CardView(RuntimeEnvironment.application);
        mosquitoCollectionCardView.setVisibility(View.GONE);
        CardView larvalBreedingCardView = new CardView(RuntimeEnvironment.application);
        larvalBreedingCardView.setVisibility(View.GONE);
        CardView paotCardview = new CardView(RuntimeEnvironment.application);
        paotCardview.setVisibility(View.GONE);

        doReturn(tvIdentifiedDate).when(activity).findViewById(eq(R.id.larval_identified_date));
        doReturn(tvLarvicideDate).when(activity).findViewById(eq(R.id.larvacide_date));
        doReturn(tvMosquitoCollectionStatus).when(activity).findViewById(eq(R.id.trap_collection_status));
        doReturn(tvMosquitoTrapSetDate).when(activity).findViewById(eq(R.id.trap_set_date));
        doReturn(tvMosquitoTrapFollowUpDate).when(activity).findViewById(eq(R.id.trap_follow_up_date));
        doReturn(tvLastUpdatedDateView).when(activity).findViewById(eq(R.id.paot_last_updated_date));
        doReturn(mosquitoCollectionCardView).when(activity).findViewById(eq( R.id.mosquito_collection_card_view));
        doReturn(larvalBreedingCardView).when(activity).findViewById(eq(R.id.larval_breeding_card_view));
        doReturn(paotCardview).when(activity).findViewById(eq(R.id.potential_area_of_transmission_card_view));
        doReturn(TEST).when(activity).getString(anyInt());

        new CardDetailsUtil().populateAndOpenMosquitoHarvestCard(mosquitoHarvestCardDetails, activity);
        assertEquals(tvMosquitoCollectionStatus.getText(), STATUS);
        assertEquals(tvMosquitoTrapSetDate.getText(), TEST + START_DATE);
        assertEquals(tvMosquitoTrapFollowUpDate.getText(), TEST + END_DATE);
        assertEquals(mosquitoCollectionCardView.getVisibility(), View.VISIBLE);

        mosquitoHarvestCardDetails = new MosquitoHarvestCardDetails(TEST, START_DATE, END_DATE, LARVAL_DIPPING);
        mosquitoHarvestCardDetails.setStatus(STATUS);

        new CardDetailsUtil().populateAndOpenMosquitoHarvestCard(mosquitoHarvestCardDetails, activity);
        assertEquals(tvIdentifiedDate.getText(), TEST + START_DATE);
        assertEquals(tvLarvicideDate.getText(), TEST + END_DATE);
        assertEquals(larvalBreedingCardView.getVisibility(), View.VISIBLE);

        mosquitoHarvestCardDetails = new MosquitoHarvestCardDetails(TEST, START_DATE, END_DATE, PAOT);

        new CardDetailsUtil().populateAndOpenMosquitoHarvestCard(mosquitoHarvestCardDetails, activity);
        assertEquals(tvLastUpdatedDateView.getText(), TEST + START_DATE);
        assertEquals(paotCardview.getVisibility(), View.VISIBLE);
    }

    @Test
    public void testBusinessStatusIsTranslatedCorrectly(){

        assertEquals(NOT_VISITED, getTranslatedBusinessStatus(NOT_VISITED));
        assertEquals(NOT_SPRAYED, getTranslatedBusinessStatus(NOT_SPRAYED));
        assertEquals(SPRAYED, getTranslatedBusinessStatus(SPRAYED));
        assertEquals(NOT_SPRAYABLE, getTranslatedBusinessStatus(NOT_SPRAYABLE));
        assertEquals(COMPLETE, getTranslatedBusinessStatus(COMPLETE));
        assertEquals(INCOMPLETE, getTranslatedBusinessStatus(INCOMPLETE));
        assertEquals(NOT_ELIGIBLE, getTranslatedBusinessStatus(NOT_ELIGIBLE));
        assertEquals(IN_PROGRESS, getTranslatedBusinessStatus(IN_PROGRESS));

    }

    @Test
    public void testIRSVerificationStatusITranslatedCorrectly(){
        assertEquals(getString(R.string.sprayed), getTranslatedIRSVerificationStatus(Constants.IRSVerificationStatus.SPRAYED));
        assertEquals(getString(R.string.not_sprayed), getTranslatedIRSVerificationStatus(Constants.IRSVerificationStatus.NOT_SPRAYED));
        assertEquals(getString(R.string.structure_not_found_or_visited_during_campaign), getTranslatedIRSVerificationStatus(Constants.IRSVerificationStatus.NOT_FOUND_OR_VISITED));
        assertEquals(getString(R.string.other), getTranslatedIRSVerificationStatus(Constants.IRSVerificationStatus.OTHER));
    }

    @Test
    public void testPopulateAndOpenIRSVerificationCardForTrueAndEligibleStructures(){

        final String TRUE_STRUCTURE = BOOLEAN_TRUE;
        final String ELIGIBLE_STRUCTURE = BOOLEAN_TRUE;
        final String REPORTED_SPRAY_STATUS = Constants.IRSVerificationStatus.SPRAYED;
        final String CHALK_SPRAY_STATUS = Constants.IRSVerificationStatus.NOT_SPRAYED;
        final String STICKER_SPRAY_STATUS = Constants.IRSVerificationStatus.NOT_FOUND_OR_VISITED;
        final String CARD_SPRAY_STATUS = Constants.IRSVerificationStatus.SPRAYED;
        final String STATUS = "status";

        Activity activity = mock(Activity.class);
        Resources resources = mock(Resources.class);

        TextView tvReportedSprayLabel = new TextView(RuntimeEnvironment.application);
        TextView tvReportedSprayStatus = new TextView(RuntimeEnvironment.application);
        TextView tvChalkSprayLabel = new TextView(RuntimeEnvironment.application);
        TextView tvChalkSprayStatus = new TextView(RuntimeEnvironment.application);
        TextView tvStickerSprayLabel = new TextView(RuntimeEnvironment.application);
        TextView tvStickerSprayStatus = new TextView(RuntimeEnvironment.application);
        TextView tvCardSprayLabel = new TextView(RuntimeEnvironment.application);
        TextView tvCardSprayStatus = new TextView(RuntimeEnvironment.application);
        TextView tvIneligibleStructuresLabel = new TextView(RuntimeEnvironment.application);
        ScrollView svEligibleStructuresScrollView = new ScrollView(RuntimeEnvironment.application);
        CardView irsVerificationCardview = new CardView(RuntimeEnvironment.application);
        irsVerificationCardview.setVisibility(View.GONE);

        doReturn(resources).when(activity).getResources();

        doReturn(tvReportedSprayLabel).when(activity).findViewById(eq(R.id.reported_spray_label));
        doReturn(tvReportedSprayStatus).when(activity).findViewById(eq(R.id.reported_spray_status));
        doReturn(tvChalkSprayLabel).when(activity).findViewById(eq(R.id.chalk_spray_label));
        doReturn(tvChalkSprayStatus).when(activity).findViewById(eq(R.id.chalk_spray_status));
        doReturn(tvStickerSprayLabel).when(activity).findViewById(eq(R.id.sticker_spray_label));
        doReturn(tvStickerSprayStatus).when(activity).findViewById(eq(R.id.sticker_spray_status));
        doReturn(tvCardSprayLabel).when(activity).findViewById(eq(R.id.card_spray_label));
        doReturn(tvCardSprayStatus).when(activity).findViewById(eq(R.id.card_spray_status));
        doReturn(tvIneligibleStructuresLabel).when(activity).findViewById(eq(R.id.ineligible_structures_label));
        doReturn(svEligibleStructuresScrollView).when(activity).findViewById(eq(R.id.eligible_structures_scrollview));
        doReturn(irsVerificationCardview).when(activity).findViewById(eq(R.id.irs_verification_card_view));

        IRSVerificationCardDetails cardDetails = new IRSVerificationCardDetails(STATUS,
                TRUE_STRUCTURE, ELIGIBLE_STRUCTURE, REPORTED_SPRAY_STATUS, CHALK_SPRAY_STATUS,
                STICKER_SPRAY_STATUS, CARD_SPRAY_STATUS);

        new CardDetailsUtil().populateAndOpenIRSVerificationCard(cardDetails, activity);

        assertEquals(getString(R.string.sprayed), tvReportedSprayStatus.getText());
        assertEquals(getString(R.string.not_sprayed), tvChalkSprayStatus.getText());
        assertEquals(getString(R.string.structure_not_found_or_visited_during_campaign), tvStickerSprayStatus.getText());
        assertEquals(getString(R.string.sprayed), tvCardSprayStatus.getText());
        assertEquals(irsVerificationCardview.getVisibility(), View.VISIBLE);

    }

    @Test
    public void testPopulateAndOpenIRSVerificationCardForNonStructures(){

        final String TRUE_STRUCTURE = BOOLEAN_FALSE;
        final String ELIGIBLE_STRUCTURE = BOOLEAN_TRUE;
        final String REPORTED_SPRAY_STATUS = Constants.IRSVerificationStatus.SPRAYED;
        final String CHALK_SPRAY_STATUS = Constants.IRSVerificationStatus.NOT_SPRAYED;
        final String STICKER_SPRAY_STATUS = Constants.IRSVerificationStatus.NOT_FOUND_OR_VISITED;
        final String CARD_SPRAY_STATUS = Constants.IRSVerificationStatus.SPRAYED;
        final String STATUS = "status";

        Activity activity = mock(Activity.class);
        Resources resources = mock(Resources.class);


        TextView tvIneligibleStructuresLabel = new TextView(RuntimeEnvironment.application);
        ScrollView svEligibleStructuresScrollView = new ScrollView(RuntimeEnvironment.application);
        CardView irsVerificationCardview = new CardView(RuntimeEnvironment.application);
        irsVerificationCardview.setVisibility(View.GONE);

        doReturn(resources).when(activity).getResources();

        doReturn(tvIneligibleStructuresLabel).when(activity).findViewById(eq(R.id.ineligible_structures_label));
        doReturn(svEligibleStructuresScrollView).when(activity).findViewById(eq(R.id.eligible_structures_scrollview));
        doReturn(irsVerificationCardview).when(activity).findViewById(eq(R.id.irs_verification_card_view));

        IRSVerificationCardDetails cardDetails = new IRSVerificationCardDetails(STATUS,
                TRUE_STRUCTURE, ELIGIBLE_STRUCTURE, REPORTED_SPRAY_STATUS, CHALK_SPRAY_STATUS,
                STICKER_SPRAY_STATUS, CARD_SPRAY_STATUS);

        new CardDetailsUtil().populateAndOpenIRSVerificationCard(cardDetails, activity);

        assertEquals(tvIneligibleStructuresLabel.getVisibility(), View.VISIBLE);
        assertEquals(svEligibleStructuresScrollView.getVisibility(), View.GONE);
        assertEquals(irsVerificationCardview.getVisibility(), View.VISIBLE);

    }


    @Test
    public void testPopulateAndOpenIRSVerificationCardForInEligibleStructures(){

        final String TRUE_STRUCTURE = BOOLEAN_TRUE;
        final String ELIGIBLE_STRUCTURE = BOOLEAN_FALSE;
        final String REPORTED_SPRAY_STATUS = Constants.IRSVerificationStatus.SPRAYED;
        final String CHALK_SPRAY_STATUS = Constants.IRSVerificationStatus.NOT_SPRAYED;
        final String STICKER_SPRAY_STATUS = Constants.IRSVerificationStatus.NOT_FOUND_OR_VISITED;
        final String CARD_SPRAY_STATUS = Constants.IRSVerificationStatus.SPRAYED;
        final String STATUS = "status";

        Activity activity = mock(Activity.class);
        Resources resources = mock(Resources.class);


        TextView tvIneligibleStructuresLabel = new TextView(RuntimeEnvironment.application);
        ScrollView svEligibleStructuresScrollView = new ScrollView(RuntimeEnvironment.application);
        CardView irsVerificationCardview = new CardView(RuntimeEnvironment.application);
        irsVerificationCardview.setVisibility(View.GONE);

        doReturn(resources).when(activity).getResources();

        doReturn(tvIneligibleStructuresLabel).when(activity).findViewById(eq(R.id.ineligible_structures_label));
        doReturn(svEligibleStructuresScrollView).when(activity).findViewById(eq(R.id.eligible_structures_scrollview));
        doReturn(irsVerificationCardview).when(activity).findViewById(eq(R.id.irs_verification_card_view));

        IRSVerificationCardDetails cardDetails = new IRSVerificationCardDetails(STATUS,
                TRUE_STRUCTURE, ELIGIBLE_STRUCTURE, REPORTED_SPRAY_STATUS, CHALK_SPRAY_STATUS,
                STICKER_SPRAY_STATUS, CARD_SPRAY_STATUS);

        new CardDetailsUtil().populateAndOpenIRSVerificationCard(cardDetails, activity);

        assertEquals(tvIneligibleStructuresLabel.getVisibility(), View.VISIBLE);
        assertEquals(svEligibleStructuresScrollView.getVisibility(), View.GONE);
        assertEquals(irsVerificationCardview.getVisibility(), View.VISIBLE);

    }

    @Test
    public void testPopulateFamilyCardShouldPopulateCorrectValues() {
        final String DATE_CREATED = "today";
        final String OWNER = "operator";
        final String STATUS = "status";
        final int STATUS_COLOR = 0;

        Activity activity = mock(Activity.class);
        Resources resources = mock(Resources.class);

        TextView tvSprayStatus = new TextView(RuntimeEnvironment.application);
        TextView tvPropertyType = new TextView(RuntimeEnvironment.application);
        TextView tvSprayDate = new TextView(RuntimeEnvironment.application);
        TextView tvSprayOperator = new TextView(RuntimeEnvironment.application);
        TextView tvFamilyHead = new TextView(RuntimeEnvironment.application);
        TextView tvReason = new TextView(RuntimeEnvironment.application);
        Button btnChangeSprayStatus = new Button(RuntimeEnvironment.application);
        Button btnRegisterFamily  =  new Button(RuntimeEnvironment.application);

        doReturn(resources).when(activity).getResources();
        doReturn(STATUS_COLOR).when(resources).getColor(anyInt());

        doReturn(tvSprayStatus).when(activity).findViewById(eq(R.id.spray_status));
        doReturn(tvPropertyType).when(activity).findViewById(eq(R.id.property_type));
        doReturn(tvSprayDate).when(activity).findViewById(eq(R.id.spray_date));
        doReturn(tvSprayOperator).when(activity).findViewById(eq(R.id.user_id));
        doReturn(tvFamilyHead).when(activity).findViewById(eq(R.id.family_head));
        doReturn(tvReason).when(activity).findViewById(eq(R.id.reason));
        doReturn(btnChangeSprayStatus).when(activity).findViewById(eq(R.id.change_spray_status));
        doReturn(btnRegisterFamily).when(activity).findViewById(eq(R.id.register_family));
        doReturn(STATUS).when(activity).getString(anyInt());

        FamilyCardDetails familyCardDetails = new FamilyCardDetails(STATUS, DATE_CREATED, OWNER);
        familyCardDetails.setStatusColor(2);
        new CardDetailsUtil().populateFamilyCard(familyCardDetails, activity);

        assertEquals(tvSprayStatus.getText(), STATUS);
        assertEquals(tvSprayDate.getText(), DATE_CREATED);
        assertEquals(tvSprayOperator.getText(), OWNER);
        assertEquals(tvSprayStatus.getCurrentTextColor(), STATUS_COLOR);

        assertEquals(btnRegisterFamily.getVisibility(), View.VISIBLE);
        assertEquals(tvPropertyType.getVisibility(), View.GONE);
        assertEquals(tvFamilyHead.getVisibility(), View.GONE);
        assertEquals(tvReason.getVisibility(), View.GONE);
        assertEquals(btnChangeSprayStatus.getVisibility(), View.GONE);
    }

}
