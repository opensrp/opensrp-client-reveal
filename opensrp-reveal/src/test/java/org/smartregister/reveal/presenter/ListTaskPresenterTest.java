package org.smartregister.reveal.presenter;

import android.content.Context;

import com.mapbox.geojson.Feature;
import com.mapbox.geojson.FeatureCollection;
import com.mapbox.mapboxsdk.geometry.LatLng;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.AdditionalMatchers;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.powermock.reflect.Whitebox;
import org.smartregister.domain.Location;
import org.smartregister.reveal.contract.BaseDrawerContract;
import org.smartregister.reveal.contract.ListTaskContract;
import org.smartregister.reveal.interactor.ListTaskInteractor;
import org.smartregister.reveal.model.CardDetails;
import org.smartregister.reveal.model.MosquitoHarvestCardDetails;
import org.smartregister.reveal.model.SprayCardDetails;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.PasswordDialogUtils;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.RevealJsonFormUtils;
import org.smartregister.reveal.util.Utils;
import org.smartregister.util.AssetHandler;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.powermock.api.mockito.PowerMockito.mock;
import static org.powermock.api.mockito.PowerMockito.mockStatic;
import static org.powermock.api.mockito.PowerMockito.spy;
import static org.powermock.api.mockito.PowerMockito.whenNew;
import static org.smartregister.reveal.interactor.ListTaskInteractorPowerMockTest.mosquitoCollectionForm;
import static org.smartregister.reveal.util.Constants.BusinessStatus.COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.INCOMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.IN_PROGRESS;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_ELIGIBLE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_VISITED;
import static org.smartregister.reveal.util.Constants.Intervention.IRS;
import static org.smartregister.reveal.util.Constants.Intervention.LARVAL_DIPPING;
import static org.smartregister.reveal.util.Constants.Intervention.MOSQUITO_COLLECTION;
import static org.smartregister.reveal.util.Constants.JsonForm.SPRAY_FORM;
import static org.smartregister.reveal.util.Constants.JsonForm.THAILAND_LARVAL_DIPPING_FORM;
import static org.smartregister.reveal.util.Constants.JsonForm.THAILAND_MOSQUITO_COLLECTION_FORM;
import static org.smartregister.reveal.util.Constants.Properties.TASK_BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.Properties.TASK_CODE;
import static org.smartregister.reveal.util.Constants.Properties.TASK_IDENTIFIER;

/**
 * @author Vincent Karuri
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest({ListTaskPresenter.class, ValidateUserLocationPresenter.class, PasswordDialogUtils.class, PreferencesUtil.class, Utils.class, AssetHandler.class, Context.class})
public class ListTaskPresenterTest {
    private ListTaskContract.ListTaskView listTaskViewSpy;
    private ListTaskPresenter listTaskPresenter;
    private ListTaskInteractor listTaskInteractor;

    @Before
    public void setUp() throws Exception {
        mockStaticMethods();

        ValidateUserLocationPresenter validateUserLocationPresenterMock = mock(ValidateUserLocationPresenter.class);
        whenNew(ValidateUserLocationPresenter.class).withAnyArguments().thenReturn(validateUserLocationPresenterMock);

        listTaskInteractor = mock(ListTaskInteractor.class);
        whenNew(ListTaskInteractor.class).withAnyArguments().thenReturn(listTaskInteractor);

        RevealJsonFormUtils jsonFormUtils = mock(RevealJsonFormUtils.class);
        when(jsonFormUtils.getFormJSON(any(), any(), any(), any(), any())).thenReturn(new JSONObject());
        listTaskViewSpy = mock(ListTaskContract.ListTaskView.class);
        when(listTaskViewSpy.getJsonFormUtils()).thenReturn(jsonFormUtils);
        listTaskPresenter = new ListTaskPresenter(listTaskViewSpy, mock(BaseDrawerContract.Presenter.class));
    }

    @Test
    public void testOnMosquitoCollectionFormSavedHidesProgressDialog() throws Exception {
        Whitebox.setInternalState(listTaskPresenter, "featureCollection", mock(FeatureCollection.class));

        listTaskPresenter.onFormSaved(null, null, null, null, IRS);
        verify(listTaskViewSpy).hideProgressDialog();
    }

    @Test
    public void testOnLocationValidatedCallsStartFormWithCorrectArgumentsForIRSEventWithNullCardDetails() {
        mockStaticMethods();

        ListTaskPresenter listTaskPresenterSpy = spy(listTaskPresenter);

        Feature feature = mock(Feature.class);
        Whitebox.setInternalState(listTaskPresenterSpy, "selectedFeature", feature);

        doNothing().when(listTaskViewSpy).startJsonForm(any(JSONObject.class));

        Whitebox.setInternalState(listTaskPresenterSpy, "selectedFeatureInterventionType", Constants.Intervention.IRS);

        RevealJsonFormUtils formUtils = mock(RevealJsonFormUtils.class);
        Whitebox.setInternalState(listTaskPresenterSpy, "jsonFormUtils", formUtils);
        doReturn(SPRAY_FORM).when(formUtils).getFormName(any(), any());

        doReturn(new JSONObject()).when(formUtils).getFormJSON(any(), any(), any(), any(), any());

        listTaskPresenterSpy.onLocationValidated();

        verify(formUtils, times(1)).getFormJSON(any(), eq(SPRAY_FORM), eq(feature), any(), any());

        verify(listTaskViewSpy).startJsonForm(any(JSONObject.class));
    }

    @Test
    public void testOnLocationValidatedCallsStartFormWithCorrectArgumentsForIRSEventWithCardDetails() {
        mockStaticMethods();

        ListTaskPresenter listTaskPresenterSpy = spy(listTaskPresenter);

        Feature feature = mock(Feature.class);
        Whitebox.setInternalState(listTaskPresenterSpy, "selectedFeature", feature);

        doNothing().when(listTaskViewSpy).startJsonForm(any(JSONObject.class));
        Whitebox.setInternalState(listTaskPresenterSpy, "selectedFeatureInterventionType", Constants.Intervention.IRS);

        CardDetails cardDetails = new SprayCardDetails(null, null, null, null, null, null);

        Whitebox.setInternalState(listTaskPresenterSpy, "cardDetails", cardDetails);

        Whitebox.setInternalState(listTaskPresenterSpy, "changeInterventionStatus", true);

        RevealJsonFormUtils formUtils = mock(RevealJsonFormUtils.class);
        Whitebox.setInternalState(listTaskPresenterSpy, "jsonFormUtils", formUtils);
        doReturn(SPRAY_FORM).when(formUtils).getFormName(any(), any());

        doReturn(new JSONObject()).when(formUtils).getFormJSON(any(), any(), any(), any(), any());

        listTaskPresenterSpy.onLocationValidated();

        verify(formUtils, times(1)).getFormJSON(any(), eq(SPRAY_FORM), eq(feature), any(), any());
        verify(listTaskViewSpy).startJsonForm(any(JSONObject.class));
    }

    @Test
    public void testOnLocationValidatedCallsStartFormWithCorrectArgumentsForMosquitoCollectionEventWithNullCardDetails() {
        mockStaticMethods();

        ListTaskPresenter listTaskPresenterSpy = spy(listTaskPresenter);

        Feature feature = mock(Feature.class);
        Whitebox.setInternalState(listTaskPresenterSpy, "selectedFeature", feature);

        doNothing().when(listTaskViewSpy).startJsonForm(any(JSONObject.class));

        Whitebox.setInternalState(listTaskPresenterSpy, "selectedFeatureInterventionType", MOSQUITO_COLLECTION);

        RevealJsonFormUtils formUtils = mock(RevealJsonFormUtils.class);
        Whitebox.setInternalState(listTaskPresenterSpy, "jsonFormUtils", formUtils);
        doReturn(THAILAND_MOSQUITO_COLLECTION_FORM).when(formUtils).getFormName(any(), any());

        doReturn(new JSONObject()).when(formUtils).getFormJSON(any(), any(), any(), any(), any());
        listTaskPresenterSpy.onLocationValidated();

        verify(formUtils, times(1)).getFormJSON(any(), eq(THAILAND_MOSQUITO_COLLECTION_FORM), eq(feature), any(), any());

        verify(listTaskViewSpy).startJsonForm(any(JSONObject.class));
    }

    @Test
    public void testOnLocationValidatedCallsStartFormWithCorrectArgumentsForMosquitoCollectionEventWithCardDetails() {
        mockStaticMethods();

        ListTaskPresenter listTaskPresenterSpy = spy(listTaskPresenter);

        Whitebox.setInternalState(listTaskPresenterSpy, "selectedFeatureInterventionType", MOSQUITO_COLLECTION);

        CardDetails mosquitoCollectionCardDetails = new MosquitoHarvestCardDetails(null, null, null, null);

        Whitebox.setInternalState(listTaskPresenterSpy, "cardDetails", mosquitoCollectionCardDetails);

        Whitebox.setInternalState(listTaskPresenterSpy, "changeInterventionStatus", true);

        Feature feature = mock(Feature.class);
        Whitebox.setInternalState(listTaskPresenterSpy, "selectedFeature", feature);

        doNothing().when(listTaskViewSpy).startJsonForm(any(JSONObject.class));

        RevealJsonFormUtils formUtils = mock(RevealJsonFormUtils.class);
        Whitebox.setInternalState(listTaskPresenterSpy, "jsonFormUtils", formUtils);
        doReturn(THAILAND_MOSQUITO_COLLECTION_FORM).when(formUtils).getFormName(any(), any());

        doReturn(new JSONObject()).when(formUtils).getFormJSON(any(), any(), any(), any(), any());

        listTaskPresenterSpy.onLocationValidated();

        verify(formUtils, times(1)).getFormJSON(any(), eq(THAILAND_MOSQUITO_COLLECTION_FORM), eq(feature), any(), any());

        verify(listTaskViewSpy).startJsonForm(any(JSONObject.class));
    }

    @Test
    public void testOnLocationValidatedCallsStartFormWithCorrectArgumentsForLarvalDippingEventWithNullCardDetails() {
        mockStaticMethods();

        ListTaskPresenter listTaskPresenterSpy = spy(listTaskPresenter);

        Feature feature = mock(Feature.class);
        Whitebox.setInternalState(listTaskPresenterSpy, "selectedFeature", feature);

        doNothing().when(listTaskViewSpy).startJsonForm(any(JSONObject.class));

        Whitebox.setInternalState(listTaskPresenterSpy, "selectedFeatureInterventionType", LARVAL_DIPPING);

        RevealJsonFormUtils formUtils = mock(RevealJsonFormUtils.class);
        Whitebox.setInternalState(listTaskPresenterSpy, "jsonFormUtils", formUtils);
        doReturn(THAILAND_LARVAL_DIPPING_FORM).when(formUtils).getFormName(any(), any());

        doReturn(new JSONObject()).when(formUtils).getFormJSON(any(), any(), any(), any(), any());

        listTaskPresenterSpy.onLocationValidated();

        verify(formUtils, times(1)).getFormJSON(any(), eq(THAILAND_LARVAL_DIPPING_FORM), eq(feature), any(), any());

        verify(listTaskViewSpy).startJsonForm(any(JSONObject.class));
    }

    @Test
    public void testOnInterventionFormDetailsFetchedShouldSetChangeSprayStatusToTrueForSprayCard() {
        Assert.assertFalse(Whitebox.getInternalState(listTaskPresenter, "changeInterventionStatus"));
        listTaskPresenter.onInterventionFormDetailsFetched(mock(SprayCardDetails.class));
        Assert.assertTrue(Whitebox.getInternalState(listTaskPresenter, "changeInterventionStatus"));
    }

    @Test
    public void testOnInterventionFormDetailsFetchedShouldSetChangeMosquitoCollectionStatusToTrueForMosquitoCollectionCard() {
        Assert.assertFalse(Whitebox.getInternalState(listTaskPresenter, "changeInterventionStatus"));
        listTaskPresenter.onInterventionFormDetailsFetched(mock(MosquitoHarvestCardDetails.class));
        Assert.assertTrue(Whitebox.getInternalState(listTaskPresenter, "changeInterventionStatus"));
    }

    @Test
    public void testOnFeatureSelectedShouldShowErrorDialogWhenTaskIdentifierIsNull() throws Exception {
        Feature feature = mock(Feature.class);

        PreferencesUtil preferencesUtil = mock(PreferencesUtil.class);
        Whitebox.setInternalState(listTaskPresenter, "prefsUtil", preferencesUtil);
        doReturn("").when(preferencesUtil).getCurrentOperationalArea();
        doNothing().when(listTaskViewSpy).displayNotification(anyString());

        final String CUSTOM_ERROR_MESSAGE = "My error message";

        Context context = mock(Context.class);
        when(listTaskViewSpy.getContext()).thenReturn(context);
        when(context.getString(anyInt(), any())).thenReturn(CUSTOM_ERROR_MESSAGE);

        Whitebox.invokeMethod(listTaskPresenter, "onFeatureSelected", feature);

        verify(listTaskViewSpy, times(1)).displayNotification(eq(CUSTOM_ERROR_MESSAGE));
    }

    @Test
    public void testfetchMosquitoCollectionDetailsIsCalledForCompleteMosquitoCollectionTask() throws Exception {
        doNothing().when(listTaskInteractor).fetchInterventionDetails(eq(MOSQUITO_COLLECTION),anyString(), anyBoolean());
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_BUSINESS_STATUS))).thenReturn(COMPLETE);
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_CODE))).thenReturn(MOSQUITO_COLLECTION);

        Feature feature = mock(Feature.class);
        doReturn(true).when(feature).hasProperty(TASK_IDENTIFIER);

        Whitebox.invokeMethod(listTaskPresenter, "onFeatureSelected", feature);

        verify(listTaskInteractor, times(1)).fetchInterventionDetails(eq(MOSQUITO_COLLECTION),AdditionalMatchers.or(anyString(), isNull()), eq(false));
    }

    @Test
    public void testfetchMosquitoCollectionDetailsIsCalledForInCompleteMosquitoCollectionTask() throws Exception {
        doNothing().when(listTaskInteractor).fetchInterventionDetails(eq(MOSQUITO_COLLECTION),anyString(), anyBoolean());
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_BUSINESS_STATUS))).thenReturn(INCOMPLETE);
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_CODE))).thenReturn(MOSQUITO_COLLECTION);

        Feature feature = mock(Feature.class);
        doReturn(true).when(feature).hasProperty(TASK_IDENTIFIER);

        Whitebox.invokeMethod(listTaskPresenter, "onFeatureSelected", feature);

        verify(listTaskInteractor, times(1)).fetchInterventionDetails(eq(MOSQUITO_COLLECTION),AdditionalMatchers.or(anyString(), isNull()), eq(false));
    }

    @Test
    public void testfetchMosquitoCollectionDetailsIsCalledForInProgressMosquitoCollectionTask() throws Exception {
        doNothing().when(listTaskInteractor).fetchInterventionDetails(eq(MOSQUITO_COLLECTION),anyString(), anyBoolean());
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_BUSINESS_STATUS))).thenReturn(IN_PROGRESS);
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_CODE))).thenReturn(MOSQUITO_COLLECTION);

        Feature feature = mock(Feature.class);
        doReturn(true).when(feature).hasProperty(TASK_IDENTIFIER);

        Whitebox.invokeMethod(listTaskPresenter, "onFeatureSelected", feature);

        verify(listTaskInteractor, times(1)).fetchInterventionDetails(eq(MOSQUITO_COLLECTION),AdditionalMatchers.or(anyString(), isNull()), eq(false));
    }

    @Test
    public void testfetchMosquitoCollectionDetailsIsCalledForNotEligibleMosquitoCollectionTask() throws Exception {
        doNothing().when(listTaskInteractor).fetchInterventionDetails(eq(MOSQUITO_COLLECTION),anyString(), anyBoolean());
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_BUSINESS_STATUS))).thenReturn(NOT_ELIGIBLE);
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_CODE))).thenReturn(MOSQUITO_COLLECTION);

        Feature feature = mock(Feature.class);
        doReturn(true).when(feature).hasProperty(TASK_IDENTIFIER);

        Whitebox.invokeMethod(listTaskPresenter, "onFeatureSelected", feature);

        verify(listTaskInteractor, times(1)).fetchInterventionDetails(eq(MOSQUITO_COLLECTION),AdditionalMatchers.or(anyString(), isNull()), eq(false));
    }

    @Test
    public void testValidateUserLocationIsCalledForInCompleteMosquitoCollectionTask() throws Exception {
        ListTaskInteractor listTaskInteractor = mock(ListTaskInteractor.class);

        ListTaskPresenter listTaskPresenter = spy(this.listTaskPresenter);

        Whitebox.setInternalState(listTaskPresenter, "listTaskInteractor", listTaskInteractor);

        doNothing().when(listTaskInteractor).fetchInterventionDetails(eq(IRS),anyString(), anyBoolean());
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_BUSINESS_STATUS))).thenReturn(NOT_VISITED);
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_CODE))).thenReturn(MOSQUITO_COLLECTION);
        doReturn(mock(android.location.Location.class)).when(listTaskViewSpy).getUserCurrentLocation();

        ValidateUserLocationPresenter locationPresenter = mock(ValidateUserLocationPresenter.class);
        Whitebox.setInternalState(listTaskPresenter, "locationPresenter", locationPresenter);
        doNothing().when(locationPresenter).requestUserLocation();
        doNothing().when(locationPresenter).onGetUserLocation(any(android.location.Location.class));

        Feature feature = mock(Feature.class);
        doReturn(true).when(feature).hasProperty(TASK_IDENTIFIER);

        Whitebox.invokeMethod(listTaskPresenter, "onFeatureSelected", feature);

        PowerMockito.verifyPrivate(listTaskPresenter, times(1)).invoke("validateUserLocation");
    }

    @Test
    public void testOpenCardViewIsCalledWithCorrectArgumentOnSprayCardDetailsFetched() {
        SprayCardDetails sprayCardDetails = new SprayCardDetails(null, null, null, null, null, null);

        doNothing().when(listTaskViewSpy).openCardView(any(CardDetails.class));
        listTaskPresenter.onCardDetailsFetched(sprayCardDetails);

        verify(listTaskViewSpy, times(1)).openCardView(eq(sprayCardDetails));
    }

    @Test
    public void testOpenCardViewIsCalledWithCorrectArgumentOnMosquitoCollectionCardDetailsFetched() {
        MosquitoHarvestCardDetails mosquitoHarvestCardDetails = new MosquitoHarvestCardDetails(null, null, null, null);

        doNothing().when(listTaskViewSpy).openCardView(any(CardDetails.class));
        listTaskPresenter.onCardDetailsFetched(mosquitoHarvestCardDetails);

        verify(listTaskViewSpy, times(1)).openCardView(eq(mosquitoHarvestCardDetails));
    }

    @Test
    public void testFetchInterventionDetailsIsCalledForChangeSprayStatus() {
        Whitebox.setInternalState(listTaskPresenter, "selectedFeature", mock(Feature.class));

        doNothing().when(listTaskViewSpy).showProgressDialog(anyInt(), anyInt());

        listTaskPresenter.onChangeInterventionStatus(IRS);

        verify(listTaskInteractor, times(1)).fetchInterventionDetails(eq(IRS), AdditionalMatchers.or(anyString(), isNull()), eq(true));
    }

    @Test
    public void testFetchInterventionDetailsIsCalledForChangeMosquitoCollectionStatus() {
        Whitebox.setInternalState(listTaskPresenter, "selectedFeature", mock(Feature.class));

        doNothing().when(listTaskViewSpy).showProgressDialog(anyInt(), anyInt());

        listTaskPresenter.onChangeInterventionStatus(MOSQUITO_COLLECTION);

        verify(listTaskInteractor, times(1)).fetchInterventionDetails(eq(MOSQUITO_COLLECTION),AdditionalMatchers.or(anyString(), isNull()), eq(true));
    }

    @Test
    public void testFetchInterventionDetailsIsCalledForRecordLarvalDipping() {
        Whitebox.setInternalState(listTaskPresenter, "selectedFeature", mock(Feature.class));

        doNothing().when(listTaskViewSpy).showProgressDialog(anyInt(), anyInt());

        listTaskPresenter.onChangeInterventionStatus(LARVAL_DIPPING);

        verify(listTaskInteractor, times(1)).fetchInterventionDetails(eq(LARVAL_DIPPING),AdditionalMatchers.or(anyString(), isNull()), eq(true));
    }

    @Test
    public void testFetchSprayDetailsIsCalledAfterSprayFormIsSaved() {
        Whitebox.setInternalState(listTaskPresenter, "featureCollection", mock(FeatureCollection.class));

        doNothing().when(listTaskViewSpy).hideProgressDialog();
        doNothing().when(listTaskViewSpy).setGeoJsonSource(any(FeatureCollection.class), any(Feature.class), anyBoolean());
        doNothing().when(listTaskInteractor).fetchInterventionDetails(eq(IRS), anyString(), anyBoolean());


        listTaskPresenter.onFormSaved(null, null, null, null, IRS);

        verify(listTaskInteractor, times(1)).fetchInterventionDetails(eq(IRS), AdditionalMatchers.or(anyString(), isNull()), eq(false));
    }

    @Test
    public void testFetchMosquitoCollectionDetailsIsCalledAfterMosquitoCollectionFormIsSaved() {
        Whitebox.setInternalState(listTaskPresenter, "featureCollection", mock(FeatureCollection.class));

        doNothing().when(listTaskViewSpy).hideProgressDialog();
        doNothing().when(listTaskViewSpy).setGeoJsonSource(any(FeatureCollection.class), any(Feature.class), anyBoolean());
        doNothing().when(listTaskInteractor).fetchInterventionDetails(eq(MOSQUITO_COLLECTION),anyString(), anyBoolean());

        listTaskPresenter.onFormSaved(null, null, null, null, MOSQUITO_COLLECTION);

        verify(listTaskInteractor, times(1)).fetchInterventionDetails(eq(MOSQUITO_COLLECTION), AdditionalMatchers.or(anyString(), isNull()), eq(false));
    }

    @Test
    public void testMaintainUsersCurrentMapCameraPositionIsSetToFalseWhenPresenterIsInitialized() throws Exception {

        Assert.assertFalse(listTaskPresenter.isChangeMapPosition());
    }

    @Test
    public void testMaintainUsersCurrentMapCameraPositionIsSetToTrueOnStructureAddedIscalled() throws Exception {

        Whitebox.setInternalState(listTaskPresenter, "featureCollection", mock(FeatureCollection.class));
        Whitebox.setInternalState(listTaskPresenter, "clickedPoint", mock(LatLng.class));

        JSONArray featureCoordinates = new JSONArray("[32.64555352892119, -14.15491759447286]");

        Assert.assertFalse(listTaskPresenter.isChangeMapPosition());

        listTaskPresenter.onStructureAdded(null, featureCoordinates);

        Assert.assertTrue(listTaskPresenter.isChangeMapPosition());
    }

    private void mockStaticMethods() {
        mockStatic(Utils.class);
        mockStatic(PreferencesUtil.class);
        mockStatic(PasswordDialogUtils.class);
        mockStatic(AssetHandler.class);

        when(AssetHandler.readFileFromAssetsFolder(AdditionalMatchers.or(isNull(), anyString()), AdditionalMatchers.or(isNull(), any(Context.class)))).thenReturn(mosquitoCollectionForm);

        PreferencesUtil preferencesUtil = mock(PreferencesUtil.class);
        PowerMockito.when(preferencesUtil.getCurrentOperationalArea()).thenReturn(new JSONObject().toString());
        PowerMockito.when(PreferencesUtil.getInstance()).thenReturn(preferencesUtil);

        PowerMockito.when(Utils.getOperationalAreaLocation(anyString())).thenReturn(new Location());
    }
}