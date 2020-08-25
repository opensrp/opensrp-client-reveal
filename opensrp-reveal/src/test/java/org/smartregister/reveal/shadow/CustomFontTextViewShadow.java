package org.smartregister.reveal.shadow;

import org.robolectric.annotation.Implements;
import org.robolectric.shadows.ShadowTextView;
import org.smartregister.view.customcontrols.CustomFontTextView;
import org.smartregister.view.customcontrols.FontVariant;

@Implements(CustomFontTextView.class)
public class CustomFontTextViewShadow extends ShadowTextView {

    public void setFontVariant(final FontVariant variant) {
        //do nothing
    }
}
