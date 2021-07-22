package org.smartregister.reveal.repository;

import android.content.Context;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

import androidx.annotation.Nullable;

import org.smartregister.AllConstants;

import java.io.File;

/**
 * Created by Richard Kareko on 7/21/21.
 */

public class MbtilesRepository extends SQLiteOpenHelper {
    public static final int DATABASE_VERSION = 6;
    public static final String DATABASE_NAME = "/data/data/org.smartregister.reveal/files/mbgl-offline.db";
    private File databasePath = new File("/data/data/org.smartregister.reveal/files/"  + AllConstants.DATABASE_NAME);

    public MbtilesRepository(Context context) {
        super(context, DATABASE_NAME, null, DATABASE_VERSION);
    }

    @Override
    public void onCreate(SQLiteDatabase sqLiteDatabase) {
        // Do nothing
    }

    @Override
    public void onUpgrade(SQLiteDatabase sqLiteDatabase, int i, int i1) {
        // Do nothing
    }
}
