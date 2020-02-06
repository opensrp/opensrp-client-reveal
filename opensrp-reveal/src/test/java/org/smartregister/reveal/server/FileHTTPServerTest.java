package org.smartregister.reveal.server;

import android.content.Context;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.robolectric.RuntimeEnvironment;
import org.smartregister.reveal.BaseUnitTest;
import org.smartregister.reveal.R;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

/**
 * Created by Richard Kareko on 2/6/20.
 */

public class FileHTTPServerTest extends BaseUnitTest {
    private Context context = RuntimeEnvironment.application;
    private FileHTTPServer httpServer;

    @Before
    public void setUp() {
        try {
            httpServer = new FileHTTPServer(context, context.getString(R.string.reveal_offline_map_download_style), "dummy_dg_id_placeholder");
            httpServer.start();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @After
    public void tearDown() {
        httpServer.destroy();
    }

    @Test
    public void testServerReturnsContent() throws Exception {
        String mapboxStyleUrl = context.getString(R.string.localhost_url, FileHTTPServer.PORT);

        String actualMapboxStyleString = getStyleJson(mapboxStyleUrl);
        assertNotNull(actualMapboxStyleString);
        assertTrue(actualMapboxStyleString.contains("\"name\": \"Reveal Style 2\""));
        assertTrue(actualMapboxStyleString.contains("metadata"));
        assertTrue(actualMapboxStyleString.contains("sources"));
        assertTrue(actualMapboxStyleString.contains("layers"));
    }

    private String getStyleJson(String urlToRead) throws Exception {
        StringBuilder result = new StringBuilder();
        URL url = new URL(urlToRead);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod("GET");
        BufferedReader rd = new BufferedReader(new InputStreamReader(conn.getInputStream()));
        String line;
        while ((line = rd.readLine()) != null) {
            result.append(line);
        }
        rd.close();
        return result.toString();
    }
}
