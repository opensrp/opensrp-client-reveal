package org.smartregister.reveal;

import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;
import org.smartregister.reveal.application.TestRevealcApplication;


@RunWith(RobolectricTestRunner.class)
@Config(application = TestRevealcApplication.class, constants = BuildConfig.class, sdk = 22)
public abstract class BaseUnitTest {

    protected static final String DUMMY_USERNAME = "myusername";
    protected static final String DUMMY_PASSWORD = "mypassword";
}
