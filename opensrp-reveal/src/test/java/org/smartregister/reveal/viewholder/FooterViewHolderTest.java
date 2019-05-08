package org.smartregister.reveal.viewholder;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;

import org.junit.Test;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;

import static org.junit.Assert.assertNotNull;

/**
 * Created by samuelgithengi on 4/24/19.
 */
public class FooterViewHolderTest extends BaseUnitTest {

    @Test
    public void testConstructor() {
        Context context = RuntimeEnvironment.application;
        View view = LayoutInflater.from(context).inflate(R.layout.smart_register_pagination, null);
        FooterViewHolder viewHolder = new FooterViewHolder(view);
        assertNotNull(viewHolder.nextPageView);
        assertNotNull(viewHolder.previousPageView);
        assertNotNull(viewHolder.pageInfoView);
    }
}
