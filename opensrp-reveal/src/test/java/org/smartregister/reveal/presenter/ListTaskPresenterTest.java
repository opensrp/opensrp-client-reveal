package org.smartregister.reveal.presenter;

import android.content.Context;

import com.mapbox.geojson.Feature;
import com.mapbox.geojson.FeatureCollection;

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
import org.smartregister.reveal.model.MosquitoCollectionCardDetails;
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
import static org.smartregister.reveal.interactor.ListTaskInteractorTest.mosquitoCollectionForm;
import static org.smartregister.reveal.util.Constants.BusinessStatus.COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.INCOMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.IN_PROGRESS;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_ELIGIBLE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.NOT_VISITED;
import static org.smartregister.reveal.util.Constants.Intervention.IRS;
import static org.smartregister.reveal.util.Constants.Intervention.MOSQUITO_COLLECTION;
import static org.smartregister.reveal.util.Constants.Properties.TASK_BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.Properties.TASK_CODE;
import static org.smartregister.reveal.util.Constants.Properties.TASK_IDENTIFIER;

/**
 * @author Vincent Karuri
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest({ListTaskPresenter.class, ValidateUserLocationPresenter.class, PasswordDialogUtils.class, PreferencesUtil.class, Utils.class, AssetHandler.class})
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

        listTaskPresenter.onFormSaved(null, null, null, IRS);

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

        listTaskPresenterSpy.onLocationValidated();

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

        Whitebox.setInternalState(listTaskPresenterSpy, "sprayCardDetails", cardDetails);

        Whitebox.setInternalState(listTaskPresenterSpy, "changeSprayStatus", true);

        listTaskPresenterSpy.onLocationValidated();

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

        listTaskPresenterSpy.onLocationValidated();

        verify(listTaskViewSpy).startJsonForm(any(JSONObject.class));
    }

    @Test
    public void testOnLocationValidatedCallsStartFormWithCorrectArgumentsForMosquitoCollectionEventWithCardDetails() {
        mockStaticMethods();

        ListTaskPresenter listTaskPresenterSpy = spy(listTaskPresenter);

        Whitebox.setInternalState(listTaskPresenterSpy, "selectedFeatureInterventionType", MOSQUITO_COLLECTION);

        CardDetails mosquitoCollectionCardDetails = new MosquitoCollectionCardDetails(null, null, null);

        Whitebox.setInternalState(listTaskPresenterSpy, "mosquitoCollectionCardDetails", mosquitoCollectionCardDetails);

        Whitebox.setInternalState(listTaskPresenterSpy, "changeMosquitoCollectionStatus", true);

        Feature feature = mock(Feature.class);
        Whitebox.setInternalState(listTaskPresenterSpy, "selectedFeature", feature);

        doNothing().when(listTaskViewSpy).startJsonForm(any(JSONObject.class));

        listTaskPresenterSpy.onLocationValidated();

        verify(listTaskViewSpy).startJsonForm(any(JSONObject.class));
    }

    @Test
    public void testOnInterventionFormDetailsFetchedShouldSetChangeSprayStatusToTrueForSprayCard() {
        Assert.assertFalse(Whitebox.getInternalState(listTaskPresenter, "changeSprayStatus"));
        listTaskPresenter.onInterventionFormDetailsFetched(mock(SprayCardDetails.class));
        Assert.assertTrue(Whitebox.getInternalState(listTaskPresenter, "changeSprayStatus"));
    }

    @Test
    public void testOnInterventionFormDetailsFetchedShouldSetChangeMosquitoCollectionStatusToTrueForMosquitoCollectionCard() {
        Assert.assertFalse(Whitebox.getInternalState(listTaskPresenter, "changeMosquitoCollectionStatus"));
        listTaskPresenter.onInterventionFormDetailsFetched(mock(MosquitoCollectionCardDetails.class));
        Assert.assertTrue(Whitebox.getInternalState(listTaskPresenter, "changeMosquitoCollectionStatus"));
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
        doReturn(context).when(listTaskViewSpy).getContext();
        doReturn(CUSTOM_ERROR_MESSAGE).when(context).getString(anyInt(), any());

        Whitebox.invokeMethod(listTaskPresenter, "onFeatureSelected", feature);

        verify(listTaskViewSpy, times(1)).displayNotification(eq(CUSTOM_ERROR_MESSAGE));
    }

    @Test
    public void testfetchMosquitoCollectionDetailsIsCalledForCompleteMosquitoCollectionTask() throws Exception {
        doNothing().when(listTaskInteractor).fetchMosquitoCollectionDetails(anyString(), anyBoolean());
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_BUSINESS_STATUS))).thenReturn(COMPLETE);
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_CODE))).thenReturn(MOSQUITO_COLLECTION);

        Feature feature = mock(Feature.class);
        doReturn(true).when(feature).hasProperty(TASK_IDENTIFIER);

        Whitebox.invokeMethod(listTaskPresenter, "onFeatureSelected", feature);

        verify(listTaskInteractor, times(1)).fetchMosquitoCollectionDetails(AdditionalMatchers.or(anyString(), isNull()), eq(false));
    }

    @Test
    public void testfetchMosquitoCollectionDetailsIsCalledForInCompleteMosquitoCollectionTask() throws Exception {
        doNothing().when(listTaskInteractor).fetchMosquitoCollectionDetails(anyString(), anyBoolean());
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_BUSINESS_STATUS))).thenReturn(INCOMPLETE);
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_CODE))).thenReturn(MOSQUITO_COLLECTION);

        Feature feature = mock(Feature.class);
        doReturn(true).when(feature).hasProperty(TASK_IDENTIFIER);

        Whitebox.invokeMethod(listTaskPresenter, "onFeatureSelected", feature);

        verify(listTaskInteractor, times(1)).fetchMosquitoCollectionDetails(AdditionalMatchers.or(anyString(), isNull()), eq(false));
    }

    @Test
    public void testfetchMosquitoCollectionDetailsIsCalledForInProgressMosquitoCollectionTask() throws Exception {
        doNothing().when(listTaskInteractor).fetchMosquitoCollectionDetails(anyString(), anyBoolean());
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_BUSINESS_STATUS))).thenReturn(IN_PROGRESS);
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_CODE))).thenReturn(MOSQUITO_COLLECTION);

        Feature feature = mock(Feature.class);
        doReturn(true).when(feature).hasProperty(TASK_IDENTIFIER);

        Whitebox.invokeMethod(listTaskPresenter, "onFeatureSelected", feature);

        verify(listTaskInteractor, times(1)).fetchMosquitoCollectionDetails(AdditionalMatchers.or(anyString(), isNull()), eq(false));
    }

    @Test
    public void testfetchMosquitoCollectionDetailsIsCalledForNotEligibleMosquitoCollectionTask() throws Exception {
        doNothing().when(listTaskInteractor).fetchMosquitoCollectionDetails(anyString(), anyBoolean());
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_BUSINESS_STATUS))).thenReturn(NOT_ELIGIBLE);
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_CODE))).thenReturn(MOSQUITO_COLLECTION);

        Feature feature = mock(Feature.class);
        doReturn(true).when(feature).hasProperty(TASK_IDENTIFIER);

        Whitebox.invokeMethod(listTaskPresenter, "onFeatureSelected", feature);

        verify(listTaskInteractor, times(1)).fetchMosquitoCollectionDetails(AdditionalMatchers.or(anyString(), isNull()), eq(false));
    }

    @Test
    public void testValidateUserLocationIsCalledForInCompleteMosquitoCollectionTask() throws Exception {
        ListTaskInteractor listTaskInteractor = mock(ListTaskInteractor.class);

        ListTaskPresenter listTaskPresenter = spy(this.listTaskPresenter);

        Whitebox.setInternalState(listTaskPresenter, "listTaskInteractor", listTaskInteractor);

        doNothing().when(listTaskInteractor).fetchMosquitoCollectionDetails(anyString(), anyBoolean());
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
        MosquitoCollectionCardDetails mosquitoCollectionCardDetails = new MosquitoCollectionCardDetails(null, null, null);

        doNothing().when(listTaskViewSpy).openCardView(any(CardDetails.class));
        listTaskPresenter.onCardDetailsFetched(mosquitoCollectionCardDetails);

        verify(listTaskViewSpy, times(1)).openCardView(eq(mosquitoCollectionCardDetails));
    }

    @Test
    public void testFetchSprayDetailsIsCalledForChangeSprayStatus() {
        Whitebox.setInternalState(listTaskPresenter, "selectedFeature", mock(Feature.class));

        doNothing().when(listTaskViewSpy).showProgressDialog(anyInt(), anyInt());

        listTaskPresenter.onChangeInterventionStatus(IRS);

        verify(listTaskInteractor, times(1)).fetchSprayDetails(AdditionalMatchers.or(anyString(), isNull()), eq(true));
    }

    @Test
    public void testFetchMosquitoCollectionDetailsIsCalledForChangeMosquitoCollectionStatus() {
        Whitebox.setInternalState(listTaskPresenter, "selectedFeature", mock(Feature.class));

        doNothing().when(listTaskViewSpy).showProgressDialog(anyInt(), anyInt());

        listTaskPresenter.onChangeInterventionStatus(MOSQUITO_COLLECTION);

        verify(listTaskInteractor, times(1)).fetchMosquitoCollectionDetails(AdditionalMatchers.or(anyString(), isNull()), eq(true));
    }

    @Test
    public void testFetchSprayDetailsIsCalledAfterSprayFormIsSaved() {
        Whitebox.setInternalState(listTaskPresenter, "featureCollection", mock(FeatureCollection.class));

        doNothing().when(listTaskViewSpy).hideProgressDialog();
        doNothing().when(listTaskViewSpy).setGeoJsonSource(any(FeatureCollection.class), any(Feature.class));
        doNothing().when(listTaskInteractor).fetchSprayDetails(anyString(), anyBoolean());

        listTaskPresenter.onFormSaved(null, null, null, IRS);

        verify(listTaskInteractor, times(1)).fetchSprayDetails(AdditionalMatchers.or(anyString(), isNull()), eq(false));
    }

    @Test
    public void testFetchMosquitoCollectionDetailsIsCalledAfterMosquitoCollectionFormIsSaved() {
        Whitebox.setInternalState(listTaskPresenter, "featureCollection", mock(FeatureCollection.class));

        doNothing().when(listTaskViewSpy).hideProgressDialog();
        doNothing().when(listTaskViewSpy).setGeoJsonSource(any(FeatureCollection.class), any(Feature.class));
        doNothing().when(listTaskInteractor).fetchMosquitoCollectionDetails(anyString(), anyBoolean());

        listTaskPresenter.onFormSaved(null, null, null, MOSQUITO_COLLECTION);

        verify(listTaskInteractor, times(1)).fetchMosquitoCollectionDetails(AdditionalMatchers.or(anyString(), isNull()), eq(false));
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
