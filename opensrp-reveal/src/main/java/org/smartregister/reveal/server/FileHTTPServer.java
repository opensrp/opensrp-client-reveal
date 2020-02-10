package org.smartregister.reveal.server;

import android.content.Context;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.util.Utils;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Locale;

import timber.log.Timber;

/**
 * A minimal HTTP server that serves a mapbox style json file
 */
public class FileHTTPServer {
    public static final int PORT = 9783;

    private static final String DEFAULT_STYLE_JSON_FILE = "map-download-style.json";
    private static final String DEFAULT_DG_ID_PLACEHOLDER = "DIGITAL_GLOBE_ID";
    private String styleJson;

    private final FileHTTPServer.ServerThread server;
    private final ServerSocket socket;

    public FileHTTPServer(Context context, String styleJsonFile, String  digitalGlobeIdPlaceHolder) throws IOException {
        socket = createBoundSocket(PORT);
        String dgIdPlaceHolder;
        if (socket == null) {
            throw new IOException("Could not find an available port");
        }
        server = new ServerThread(socket);

        dgIdPlaceHolder = StringUtils.isNotBlank(digitalGlobeIdPlaceHolder) ? digitalGlobeIdPlaceHolder : DEFAULT_DG_ID_PLACEHOLDER;
        styleJson = StringUtils.isNotBlank(styleJsonFile) ? styleJsonFile : DEFAULT_STYLE_JSON_FILE;

        styleJson = Utils.readAssetContents(context, styleJsonFile);
        styleJson = styleJson.replace(dgIdPlaceHolder, BuildConfig.DG_CONNECT_ID);
    }

    public void start() {
        server.start();
    }

    public boolean isStarted() {
        return server.isAlive();
    }

    /**
     * Permanently closes all sockets.
     */
    public void destroy() {
        try {
            socket.close();
        } catch (IOException e) {
            Timber.w(e);
        }
        server.interrupt();
    }

    /**
     * Binds a ServerSocket to given port.
     */
    protected static ServerSocket createBoundSocket(int port) throws IOException {
        try {
            return new ServerSocket(port);
        } catch (IOException e) {
            e.printStackTrace();
        }
        Timber.e("Port %d not available", port);
        return null;
    }

    class ServerThread extends Thread {
        private final ServerSocket socket;

        ServerThread(ServerSocket socket) {
            this.socket = socket;
        }

        public void run() {
            try {
                socket.setReuseAddress(true);
                Timber.i("Ready for requests on port %d", socket.getLocalPort());
                while (!isInterrupted()) {
                    Socket connection = socket.accept();
                    Timber.i("Accepted a client connection");
                    new ResponseThread(connection).start();
                }
                Timber.i("Server thread interrupted");
            } catch (IOException e) {
                Timber.i("Server thread stopped: %s", e.getMessage());
            }
        }
    }

    class ResponseThread extends Thread {
        private final Socket connection;

        ResponseThread(Socket connection) {
            this.connection = connection;
        }

        public void run() {
            try (Socket connection = this.connection) {
                InputStreamReader reader = new InputStreamReader(connection.getInputStream());
                String request = new BufferedReader(reader).readLine();
                Timber.i("Received request: %s", request);
                if (request == null) {
                    return;
                }
                long start = System.currentTimeMillis();
                FileHTTPServer.Response response = getResponse(request);
                if (response == null) {
                    Timber.i("%s: Style file not found", request);
                    return;
                }
                sendResponse(connection, response);
                long finish = System.currentTimeMillis();
                Timber.i("%s: Served %d bytes in %d ms", request, response.data.length, finish - start);
            } catch (IOException e) {
                Timber.e(e, "Unable to read request from socket");
            }
        }

        protected FileHTTPServer.Response getResponse(String request) {
            if (request.startsWith("GET /")) {
                return new FileHTTPServer.Response(styleJson.getBytes(),  "text/plain");
            } else {
                Timber.w("Ignoring request: %s", request);
                return null;
            }

        }

        protected void sendResponse(Socket connection, FileHTTPServer.Response response) {
            String headers = String.format(
                    Locale.US,
                    "HTTP/1.0 200\r\n" +
                            "Content-Type: %s\r\n" +
                            "Content-Length: %d\r\n" +
                            "\r\n",
                    response.contentType,
                    response.data.length
            );

            try (OutputStream output = connection.getOutputStream()) {
                output.write(headers.getBytes());
                output.write(response.data);
                output.flush();
            } catch (IOException e) {
                Timber.e(e, "Unable to write response to socket");
            }
        }
    }

    public static class Response {
        private byte[] data;
        private String contentType;

        public Response(byte[] data, String contentType) {
            this.data = data;
            this.contentType = contentType;
        }
    }

}
