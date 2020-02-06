package org.smartregister.reveal.server;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.reveal.BuildConfig;
import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.util.Utils;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Date;
import java.util.StringTokenizer;

public class JavaHTTPServer implements Runnable{

    private static final String DEFAULT_STYLE_JSON_FILE = "map-download-style.json";
    private static final String DEFAULT_DG_ID_PLACEHOLDER = "DIGITAL_GLOBE_ID";
    public static String digitalGlobeIdPlaceHolder;
    public static String styleJsonFile;
    private static String styleJson;
    // port to listen connection
    public static final int PORT = 8783;

    // verbose mode
    private static final boolean verbose = true;

    // Client Connection via Socket Class
    private Socket connect;

    public JavaHTTPServer(Socket c) {
        connect = c;
    }

    public static void init() {
        try {
            ServerSocket serverConnect = new ServerSocket(PORT);
            System.out.println("Server started.\nListening for connections on port : " + PORT + " ...\n");

            digitalGlobeIdPlaceHolder = StringUtils.isNotBlank(digitalGlobeIdPlaceHolder) ? digitalGlobeIdPlaceHolder : DEFAULT_DG_ID_PLACEHOLDER;
            styleJsonFile = StringUtils.isNotBlank(styleJsonFile) ? styleJsonFile : DEFAULT_STYLE_JSON_FILE;

            styleJson = Utils.readAssetContents(RevealApplication.getInstance().getApplicationContext(), styleJsonFile);
            styleJson = styleJson.replace(digitalGlobeIdPlaceHolder, BuildConfig.DG_CONNECT_ID);

            // we listen until user halts server execution
            while (true) {
                JavaHTTPServer myServer = new JavaHTTPServer(serverConnect.accept());

                if (verbose) {
                    System.out.println("Connecton opened. (" + new Date() + ")");
                }

                // create dedicated thread to manage the client connection
                Thread thread = new Thread(myServer);
                thread.start();
            }


        } catch (IOException e) {
            System.err.println("Server Connection error : " + e.getMessage());
        }
    }

    @Override
    public void run() {
        // we manage our particular client connection
        BufferedReader in = null; PrintWriter out = null; BufferedOutputStream dataOut = null;
        String fileRequested;

        try {
            // we read characters from the client via input stream on the socket
            in = new BufferedReader(new InputStreamReader(connect.getInputStream()));
            // we get character output stream to client (for headers)
            out = new PrintWriter(connect.getOutputStream());
            // get binary output stream to client (for requested data)
            dataOut = new BufferedOutputStream(connect.getOutputStream());

            // get first line of the request from the client
            String input = in.readLine();
            // we parse the request with a string tokenizer
            StringTokenizer parse = new StringTokenizer(input);
            String method = parse.nextToken().toUpperCase(); // we get the HTTP method of the client

            fileRequested = styleJsonFile;

            int fileLength = styleJson.getBytes().length;
            String content = getContentType(fileRequested);

            if ("GET".equals(method)) { // GET method so we return content
                byte[] fileData = styleJson.getBytes();

                // send HTTP Headers
                out.println("HTTP/1.1 200 OK");
                out.println("Server: Java HTTP Server from SSaurel : 1.0");
                out.println("Date: " + new Date());
                out.println("Content-type: " + content);
                out.println("Content-length: " + fileLength);
                out.println(); // blank line between headers and content, very important !
                out.flush(); // flush character output stream buffer


                dataOut.write(fileData, 0, fileLength);
                dataOut.flush();
            }

            if (verbose) {
                System.out.println("File " + fileRequested + " of type " + content + " returned");
            }


        } catch (IOException ioe) {
            System.err.println("Server error : " + ioe);
        } finally {
            try {
                in.close();
                out.close();
                dataOut.close();
                connect.close(); // we close socket connection
            } catch (Exception e) {
                System.err.println("Error closing stream : " + e.getMessage());
            }

            if (verbose) {
                System.out.println("Connection closed.\n");
            }
        }


    }

    // return supported MIME Types
    private String getContentType(String fileRequested) {
        if (fileRequested.endsWith(".htm")  ||  fileRequested.endsWith(".html"))
            return "text/html";
        else
            return "text/plain";
    }

}
