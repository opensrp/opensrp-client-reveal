package org.smartregister.reveal;

import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;
import org.smartregister.reveal.application.TestRevealcApplication;

import java.util.Calendar;
import java.util.Date;

/**
 * Created by ndegwamartin on 27/03/2018.
 */

@RunWith(RobolectricTestRunner.class)
@Config(application = TestRevealcApplication.class, constants = BuildConfig.class, sdk = 22)
public abstract class BaseUnitTest {

    protected static final String DUMMY_USERNAME = "myusername";
    protected static final String DUMMY_PASSWORD = "mypassword";
    protected static final String DUMMY_BASE_ENTITY_ID = "00ts-ime-hcla-0tib-0eht-ma0i";
    protected static final String TEST_STRING = "teststring";
    protected static final String WHO_ANC_ID = "12345678";
    protected static final String GLOBAL_IDENTIFIER = "identifier";
    protected static final String NULL_STRING = null;
    protected static final long DUMMY_LONG = 1000l;
    protected static final Date DUMMY_DATE = Calendar.getInstance().getTime();
    protected static final String DUMMY_JSON = "[\r\n        {\r\n            \"key\": \"site_ipv_assess\",\r\n            \"label\": \"Minimum requirements for IPV assessment\",\r\n            \"value\": \"true\",\r\n            \"description\": \"\\\"Are all of the following in place at your facility: \\r\\n1. A protocol or standard operating procedure for Intimate Partner Violence (IPV); \\r\\n2. A health worker trained on how to ask about IPV and how to provide the minimum response or beyond;\\r\\n3. A private setting; \\r\\n4. A way to ensure confidentiality; \\r\\n5. Time to allow for appropriate disclosure; and\\r\\n6. A system for referral in place. \\\"\"\r\n        },\r\n        {\r\n            \"key\": \"site_anc_hiv\",\r\n            \"label\": \"Generalized HIV epidemic\",\r\n            \"value\": \"true\",\r\n            \"description\": \"Is the HIV prevalence consistently > 1% in pregnant women attending antenatal clinics at your facility?\"\r\n        },\r\n        {\r\n            \"key\": \"site_ultrasound\",\r\n            \"label\": \"Ultrasound available\",\r\n            \"value\": \"false\",\r\n            \"description\": \"Is an ultrasound machine available and functional at your facility and a trained health worker available to use it?\"\r\n        },\r\n        {\r\n            \"key\": \"site_bp_tool\",\r\n            \"label\": \"Automated BP measurement tool\",\r\n            \"value\": \"true\",\r\n            \"description\": \"Does your facility use an automated blood pressure (BP) measurement tool?\"\r\n        }\r\n    ]";
    public static int ASYNC_TIMEOUT = 1000;

}
