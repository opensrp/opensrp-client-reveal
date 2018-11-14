package org.smartregister.reveal.presenter;

import android.graphics.Color;
import android.graphics.drawable.GradientDrawable;
import android.util.Log;
import android.view.View;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.TextView;

import com.android.volley.toolbox.ImageLoader;

import org.smartregister.configurableviews.model.LoginConfiguration;
import org.smartregister.configurableviews.model.ViewConfiguration;
import org.smartregister.login.model.BaseLoginModel;
import org.smartregister.login.presenter.BaseLoginPresenter;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.interactor.LoginInteractor;
import org.smartregister.reveal.util.Constants;
import org.smartregister.reveal.util.ImageLoaderRequest;
import org.smartregister.ug.reveal.R;
import org.smartregister.view.contract.BaseLoginContract;

import java.lang.ref.WeakReference;


/**
 * Created by ndegwamartin on 22/06/2018.
 */
public class LoginPresenter extends BaseLoginPresenter implements BaseLoginContract.Presenter {

    private static final String TAG = LoginPresenter.class.getCanonicalName();

    public LoginPresenter(BaseLoginContract.View loginView) {
        mLoginView = new WeakReference<>(loginView);
        mLoginInteractor = new LoginInteractor(this);
        mLoginModel = new BaseLoginModel();
    }

    @Override
    public void processViewCustomizations() {
        try {
            String jsonString = getJsonViewFromPreference(Constants.VIEW_CONFIGURATION_PREFIX + Constants.CONFIGURATION.LOGIN);
            if (jsonString == null) {
                return;
            }

            ViewConfiguration loginView = RevealApplication.getJsonSpecHelper().getConfigurableView(jsonString);
            LoginConfiguration metadata = (LoginConfiguration) loginView.getMetadata();
            LoginConfiguration.Background background = metadata.getBackground();

            CheckBox showPasswordCheckBox = getLoginView().getActivityContext().findViewById(R.id.login_show_password_checkbox);
            TextView showPasswordTextView = getLoginView().getActivityContext().findViewById(R.id.login_show_password_text_view);
            if (!metadata.getShowPasswordCheckbox()) {
                showPasswordCheckBox.setVisibility(View.GONE);
                showPasswordTextView.setVisibility(View.GONE);
            } else {
                showPasswordCheckBox.setVisibility(View.VISIBLE);
                showPasswordTextView.setVisibility(View.VISIBLE);
            }

            if (background.getOrientation() != null && background.getStartColor() != null && background.getEndColor() != null) {
                View loginLayout = getLoginView().getActivityContext().findViewById(R.id.login_layout);
                GradientDrawable gradientDrawable = new GradientDrawable();
                gradientDrawable.setShape(GradientDrawable.RECTANGLE);
                gradientDrawable.setOrientation(
                        GradientDrawable.Orientation.valueOf(background.getOrientation()));
                gradientDrawable.setColors(new int[]{Color.parseColor(background.getStartColor()),
                        Color.parseColor(background.getEndColor())});
                loginLayout.setBackground(gradientDrawable);
            }

            if (metadata.getLogoUrl() != null) {
                ImageView imageView = getLoginView().getActivityContext().findViewById(R.id.login_logo);
                ImageLoaderRequest.getInstance(getLoginView().getActivityContext()).getImageLoader()
                        .get(metadata.getLogoUrl(), ImageLoader.getImageListener(imageView,
                                R.drawable.ic_who_logo, R.drawable.ic_who_logo)).getBitmap();
            }

        } catch (Exception e) {
            Log.d(TAG, e.getMessage());
        }
    }

    @Override
    public boolean isSiteCharacteristicsSet() {
        return false;
    }
}
