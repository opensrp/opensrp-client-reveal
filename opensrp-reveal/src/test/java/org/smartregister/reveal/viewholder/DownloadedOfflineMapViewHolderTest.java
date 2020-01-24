package org.smartregister.reveal.viewholder;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.CheckBox;
import android.widget.TextView;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.mockito.junit.MockitoJUnit;
import org.mockito.junit.MockitoRule;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;
import org.smartregister.reveal.model.OfflineMapModel;

import java.util.Date;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.smartregister.reveal.model.OfflineMapModel.OfflineMapStatus.DOWNLOADED;

/**
 * Created by Richard Kareko on 1/24/20.
 */

public class DownloadedOfflineMapViewHolderTest extends BaseUnitTest {

    @Rule
    public MockitoRule rule = MockitoJUnit.rule();

    private DownloadedOfflineMapViewHolder viewHolder;

    private Context context = RuntimeEnvironment.application;

    @Before
    public void setup() {
        View view = LayoutInflater.from(context).inflate(R.layout.offline_map_row, null);
        viewHolder = new DownloadedOfflineMapViewHolder(view);
    }

    @Test
    public void testSetOfflineMapLabel() {
        viewHolder.setOfflineMapLabel("Akros_1");
        assertEquals("Akros_1",  ((TextView) viewHolder.itemView.findViewById(R.id.offline_map_label)).getText());
    }

    @Test
    public void testSetDownloadedMapSize() {
        viewHolder.setDownloadedMapSize("100 MB");
        assertEquals("100 MB",  ((TextView) viewHolder.itemView.findViewById(R.id.downloading_label)).getText());
    }

    @Test
    public void testSetItemViewListener() {
        OfflineMapModel expectedOfflineMapModel = new OfflineMapModel();
        Date dateCreated = new Date();
        expectedOfflineMapModel.setDateCreated(dateCreated);
        expectedOfflineMapModel.setOfflineMapStatus(DOWNLOADED);

        View.OnClickListener onClickListener = new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                // do nothing
            }
        };

        viewHolder.setItemViewListener(expectedOfflineMapModel, onClickListener);

        CheckBox actualCheckBox =  viewHolder.itemView.findViewById(R.id.offline_map_checkbox);
        assertNotNull(actualCheckBox);
        OfflineMapModel actualOfflineMapModel = (OfflineMapModel) actualCheckBox.getTag(R.id.offline_map_checkbox);
        assertNotNull(actualOfflineMapModel);

        assertEquals(dateCreated, actualOfflineMapModel.getDateCreated());
        assertEquals(DOWNLOADED, actualOfflineMapModel.getOfflineMapStatus());
    }

    @Test
    public void testCheckCheckBox() {

        viewHolder.checkCheckBox(true);

        CheckBox actualCheckBox =  viewHolder.itemView.findViewById(R.id.offline_map_checkbox);
        assertNotNull(actualCheckBox);
        assertTrue(actualCheckBox.isChecked());
    }

    @Test
    public void testUnCheckCheckBox() {

        viewHolder.checkCheckBox(false);

        CheckBox actualCheckBox =  viewHolder.itemView.findViewById(R.id.offline_map_checkbox);
        assertNotNull(actualCheckBox);
        assertFalse(actualCheckBox.isChecked());
    }

    @Test
    public void testDisplayDownloadSizeLabel() {

        viewHolder.displayDownloadSizeLabel(true);

        TextView downloadingLabel = viewHolder.itemView.findViewById(R.id.offline_map_label);
        assertNotNull(downloadingLabel);
        assertEquals(View.VISIBLE, downloadingLabel.getVisibility());
    }

    @Test
    public void testHideDownloadSizeLabel() {

        viewHolder.displayDownloadSizeLabel(false);

        TextView downloadingLabel = viewHolder.itemView.findViewById(R.id.offline_map_label);
        assertNotNull(downloadingLabel);
    }


}
