package org.smartregister.reveal.view;

import android.content.Intent;

import org.junit.Before;
import org.junit.Test;
import org.powermock.reflect.Whitebox;
import org.robolectric.Robolectric;
import org.robolectric.util.ReflectionHelpers;
import org.smartregister.Context;
import org.smartregister.CoreLibrary;
import org.smartregister.family.util.Constants;
import org.smartregister.family.util.Constants.INTENT_KEY;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.presenter.FamilyOtherMemberPresenter;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.smartregister.reveal.util.Constants.DatabaseKeys.STRUCTURE_ID;

/**
 * Created by samuelgithengi on 1/26/21.
 */
public class FamilyOtherMemberProfileActivityTest extends BaseUnitTest {

    private FamilyOtherMemberProfileActivity activity;

    @Before
    public void setUp() {
        Context.bindtypes = new ArrayList<>();
        //ReflectionHelpers.setField(Context.getInstance(), "ziggyService", ziggyService);
        Intent intent = new Intent();
        intent.putExtra(INTENT_KEY.BASE_ENTITY_ID, "1d1232");
        intent.putExtra(INTENT_KEY.FAMILY_BASE_ENTITY_ID, "f_1d1232");
        intent.putExtra(INTENT_KEY.FAMILY_HEAD, "doe");
        intent.putExtra(INTENT_KEY.PRIMARY_CAREGIVER, "PRIMARY_CAREGIVER");
        intent.putExtra(INTENT_KEY.FAMILY_NAME, "Dolce");
        intent.putExtra(STRUCTURE_ID, "s_122323");
        activity = Robolectric.buildActivity(FamilyOtherMemberProfileActivity.class, intent).get();
    }

    @Test
    public void testInitializePresenterShouldInitializePresenter() {
        assertNull(activity.presenter());
        activity.initializePresenter();
        assertNotNull(activity.presenter());
        assertFalse(ReflectionHelpers.getField(activity, "isFamilyHead"));
        FamilyOtherMemberPresenter presenter= (FamilyOtherMemberPresenter) activity.presenter();
        assertEquals("1d1232",presenter.getBaseEntityId());
        assertEquals("f_1d1232", ReflectionHelpers.getField(presenter,"familyBaseEntityId"));
        assertEquals("Dolce", ReflectionHelpers.getField(presenter,"familyName"));

    }
}
