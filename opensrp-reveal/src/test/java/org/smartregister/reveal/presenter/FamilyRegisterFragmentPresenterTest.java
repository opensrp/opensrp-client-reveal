package org.smartregister.reveal.presenter;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.smartregister.family.contract.FamilyRegisterFragmentContract;
import org.smartregister.reveal.BaseUnitTest;

import static org.junit.Assert.assertEquals;

/**
 * Created by samuelgithengi on 4/25/19.
 */
public class FamilyRegisterFragmentPresenterTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    @Mock
    private FamilyRegisterFragmentContract.View view;

    @Mock
    private FamilyRegisterFragmentContract.Model model;

    private FamilyRegisterFragmentPresenter presenter;

    @Before
    public void setUp() {
        presenter = new FamilyRegisterFragmentPresenter(view, model, null);
    }

    @Test
    public void tesGetMainCondition() {
        assertEquals(" date_removed is null ", presenter.getMainCondition());
    }

    @Test
    public void testGetDefaultSortQuery() {
        assertEquals("last_interacted_with DESC ", presenter.getDefaultSortQuery());
    }

}
