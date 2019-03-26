package org.smartregister.reveal.presenter;

import android.content.Context;
import android.service.autofill.Validators;

import com.mapbox.geojson.Feature;
import com.mapbox.geojson.FeatureCollection;
import com.mapbox.geojson.Geometry;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.AdditionalMatchers;
import org.powermock.api.mockito.PowerMockito;
import org.powermock.core.classloader.annotations.PrepareForTest;
import org.powermock.modules.junit4.PowerMockRunner;
import org.powermock.reflect.Whitebox;
import org.smartregister.reveal.contract.ListTaskContract;
import org.smartregister.reveal.interactor.ListTaskInteractor;
import org.smartregister.reveal.model.CardDetails;
import org.smartregister.reveal.model.MosquitoCollectionCardDetails;
import org.smartregister.reveal.model.SprayCardDetails;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.PasswordDialogUtils;
import org.smartregister.reveal.util.PreferencesUtil;
import org.smartregister.reveal.util.Utils;

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
import static org.smartregister.reveal.util.Constants.BusinessStatus.COMPLETE;
import static org.smartregister.reveal.util.Constants.BusinessStatus.INCOMPLETE;
import static org.smartregister.reveal.util.Constants.Intervention.IRS;
import static org.smartregister.reveal.util.Constants.Intervention.MOSQUITO_COLLECTION;
import static org.smartregister.reveal.util.Constants.Properties.TASK_BUSINESS_STATUS;
import static org.smartregister.reveal.util.Constants.Properties.TASK_CODE;
import static org.smartregister.reveal.util.Constants.Properties.TASK_IDENTIFIER;

/**
 * @author Vincent Karuri
 */
@RunWith(PowerMockRunner.class)
@PrepareForTest({ListTaskPresenter.class, ValidateUserLocationPresenter.class, PasswordDialogUtils.class, PreferencesUtil.class, Utils.class})
public class ListTaskPresenterTest {
    private ListTaskContract.ListTaskView listTaskViewSpy;
    private ListTaskPresenter listTaskPresenter;

    @Before
    public void setUp() throws Exception {
        mockStatic(PasswordDialogUtils.class);
        mockStatic(PreferencesUtil.class);

        ListTaskInteractor listTaskInteractorMock = mock(ListTaskInteractor.class);
        whenNew(ListTaskInteractor.class).withAnyArguments().thenReturn(listTaskInteractorMock);

        ValidateUserLocationPresenter validateUserLocationPresenterMock = mock(ValidateUserLocationPresenter.class);
        whenNew(ValidateUserLocationPresenter.class).withAnyArguments().thenReturn(validateUserLocationPresenterMock);

        ListTaskContract.ListTaskView listTaskView = mock(ListTaskContract.ListTaskView.class);
        listTaskViewSpy = spy(listTaskView);
        listTaskPresenter = new ListTaskPresenter(listTaskViewSpy);
    }

    @Test
    public void testOnMosquitoCollectionFormSavedHidesProgressDialog() throws Exception {
        Whitebox.setInternalState(listTaskPresenter, "featureCollection", mock(FeatureCollection.class));

        listTaskPresenter.onFormSaved(null, null, null, Constants.Intervention.IRS);

        verify(listTaskViewSpy).hideProgressDialog();
    }

    @Test
    public void testOnLocationValidatedCallsStartFormWithCorrectArgumentsForIRSEventWithNullCardDetails() throws Exception {

        ListTaskPresenter listTaskPresenterSpy = spy(listTaskPresenter);

        PowerMockito.doNothing().when(listTaskPresenterSpy, "startForm", any(), any(), anyString());

        Whitebox.setInternalState(listTaskPresenterSpy, "selectedFeatureInterventionType", Constants.Intervention.IRS);

        listTaskPresenterSpy.onLocationValidated();

        PowerMockito.verifyPrivate(listTaskPresenterSpy, times(1)).invoke("startForm", isNull(), isNull(), eq(Constants.SPRAY_EVENT));
    }

    @Test
    public void testOnLocationValidatedCallsStartFormWithCorrectArgumentsForIRSEventWithCardDetails() throws Exception {

        ListTaskPresenter listTaskPresenterSpy = spy(listTaskPresenter);

        PowerMockito.doNothing().when(listTaskPresenterSpy, "startForm", any(), any(), anyString());

        Whitebox.setInternalState(listTaskPresenterSpy, "selectedFeatureInterventionType", Constants.Intervention.IRS);

        CardDetails cardDetails = new SprayCardDetails(null, null, null, null, null, null);

        Whitebox.setInternalState(listTaskPresenterSpy, "sprayCardDetails", cardDetails);

        Whitebox.setInternalState(listTaskPresenterSpy, "changeSprayStatus", true);

        listTaskPresenterSpy.onLocationValidated();

        PowerMockito.verifyPrivate(listTaskPresenterSpy, times(1)).invoke("startForm", isNull(), eq(cardDetails), eq(Constants.SPRAY_EVENT));
    }

    @Test
    public void testOnLocationValidatedCallsStartFormWithCorrectArgumentsForMosquitoCollectionEventWithNullCardDetails() throws Exception {

        ListTaskPresenter listTaskPresenterSpy = spy(listTaskPresenter);

        PowerMockito.doNothing().when(listTaskPresenterSpy, "startForm", any(), any(), anyString());

        Whitebox.setInternalState(listTaskPresenterSpy, "selectedFeatureInterventionType", MOSQUITO_COLLECTION);

        listTaskPresenterSpy.onLocationValidated();

        PowerMockito.verifyPrivate(listTaskPresenterSpy, times(1)).invoke("startForm", isNull(), isNull(), eq(Constants.MOSQUITO_COLLECTION_EVENT));
    }

    @Test
    public void testOnLocationValidatedCallsStartFormWithCorrectArgumentsForMosquitoCollectionEventWithCardDetails() throws Exception {

        ListTaskPresenter listTaskPresenterSpy = spy(listTaskPresenter);

        PowerMockito.doNothing().when(listTaskPresenterSpy, "startForm", any(), any(), anyString());

        Whitebox.setInternalState(listTaskPresenterSpy, "selectedFeatureInterventionType", MOSQUITO_COLLECTION);

        CardDetails mosquitoCollectionCardDetails = new MosquitoCollectionCardDetails(null, null, null);

        Whitebox.setInternalState(listTaskPresenterSpy, "mosquitoCollectionCardDetails", mosquitoCollectionCardDetails);

        Whitebox.setInternalState(listTaskPresenterSpy, "changeMosquitoCollectionStatus", true);

        listTaskPresenterSpy.onLocationValidated();

        PowerMockito.verifyPrivate(listTaskPresenterSpy, times(1)).invoke("startForm", isNull(), eq(mosquitoCollectionCardDetails), eq(Constants.MOSQUITO_COLLECTION_EVENT));
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
        ListTaskInteractor listTaskInteractor = mock(ListTaskInteractor.class);
        Whitebox.setInternalState(listTaskPresenter, "listTaskInteractor", listTaskInteractor);

        mockStatic(Utils.class);

        doNothing().when(listTaskInteractor).fetchMosquitoCollectionDetails(anyString(), anyBoolean());
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_BUSINESS_STATUS))).thenReturn(COMPLETE);
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_CODE))).thenReturn(MOSQUITO_COLLECTION);

        Feature feature = mock(Feature.class);
        doReturn(true).when(feature).hasProperty(TASK_IDENTIFIER);

        Whitebox.invokeMethod(listTaskPresenter, "onFeatureSelected", feature);

        verify(listTaskInteractor, times(1)).fetchMosquitoCollectionDetails(AdditionalMatchers.or(anyString(), isNull()), eq(false));
    }

    @Test
    public void testvalidateUserLocationIsCalledForInCompleteMosquitoCollectionTask() throws Exception {
        ListTaskInteractor listTaskInteractor = mock(ListTaskInteractor.class);

        ListTaskPresenter listTaskPresenter = spy(this.listTaskPresenter);

        Whitebox.setInternalState(listTaskPresenter, "listTaskInteractor", listTaskInteractor);

        mockStatic(Utils.class);

        doNothing().when(listTaskInteractor).fetchMosquitoCollectionDetails(anyString(), anyBoolean());
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_BUSINESS_STATUS))).thenReturn(INCOMPLETE);
        when(Utils.getPropertyValue(any(Feature.class), eq(TASK_CODE))).thenReturn(MOSQUITO_COLLECTION);
        PowerMockito.doNothing().when(listTaskPresenter, "validateUserLocation");

        Feature feature = mock(Feature.class);
        doReturn(true).when(feature).hasProperty(TASK_IDENTIFIER);

        Whitebox.invokeMethod(listTaskPresenter, "onFeatureSelected", feature);

        PowerMockito.verifyPrivate(listTaskPresenter,  times(1)).invoke("validateUserLocation");
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
        ListTaskInteractor listTaskInteractor = mock(ListTaskInteractor.class);

        Whitebox.setInternalState(listTaskPresenter, "listTaskInteractor", listTaskInteractor);
        Whitebox.setInternalState(listTaskPresenter, "selectedFeature", mock(Feature.class));

        doNothing().when(listTaskViewSpy).showProgressDialog(anyInt(), anyInt());

        listTaskPresenter.onChangeInterventionStatus(IRS);

        verify(listTaskInteractor, times(1)).fetchSprayDetails(AdditionalMatchers.or(anyString(), isNull()), eq(true));
    }

    @Test
    public void testFetchMosquitoCollectionDetailsIsCalledForChangeMosquitoCollectionStatus() {
        ListTaskInteractor listTaskInteractor = mock(ListTaskInteractor.class);

        Whitebox.setInternalState(listTaskPresenter, "listTaskInteractor", listTaskInteractor);
        Whitebox.setInternalState(listTaskPresenter, "selectedFeature", mock(Feature.class));

        doNothing().when(listTaskViewSpy).showProgressDialog(anyInt(), anyInt());

        listTaskPresenter.onChangeInterventionStatus(MOSQUITO_COLLECTION);

        verify(listTaskInteractor, times(1)).fetchMosquitoCollectionDetails(AdditionalMatchers.or(anyString(), isNull()), eq(true));
    }

    @Test
    public void testFetchSprayDetailsIsCalledAfterSprayFormIsSaved() {
        ListTaskInteractor listTaskInteractor = mock(ListTaskInteractor.class);

        Whitebox.setInternalState(listTaskPresenter, "listTaskInteractor", listTaskInteractor);
        Whitebox.setInternalState(listTaskPresenter, "featureCollection", mock(FeatureCollection.class));

        doNothing().when(listTaskViewSpy).hideProgressDialog();
        doNothing().when(listTaskViewSpy).setGeoJsonSource(any(FeatureCollection.class), any(Geometry.class));
        doNothing().when(listTaskInteractor).fetchSprayDetails(anyString(), anyBoolean());

        listTaskPresenter.onFormSaved(null, null, null, IRS);

        verify(listTaskInteractor, times(1)).fetchSprayDetails(AdditionalMatchers.or(anyString(), isNull()), eq(false));
    }

    @Test
    public void testFetchMosquitoCollectionDetailsIsCalledAfterMosquitoCollectionFormIsSaved() {
        ListTaskInteractor listTaskInteractor = mock(ListTaskInteractor.class);

        Whitebox.setInternalState(listTaskPresenter, "listTaskInteractor", listTaskInteractor);
        Whitebox.setInternalState(listTaskPresenter, "featureCollection", mock(FeatureCollection.class));

        doNothing().when(listTaskViewSpy).hideProgressDialog();
        doNothing().when(listTaskViewSpy).setGeoJsonSource(any(FeatureCollection.class), any(Geometry.class));
        doNothing().when(listTaskInteractor).fetchMosquitoCollectionDetails(anyString(), anyBoolean());

        listTaskPresenter.onFormSaved(null, null, null, MOSQUITO_COLLECTION);

        verify(listTaskInteractor, times(1)).fetchMosquitoCollectionDetails(AdditionalMatchers.or(anyString(), isNull()), eq(false));
    }
}
