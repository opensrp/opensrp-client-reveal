package org.smartregister.reveal.shadow;

import android.content.Context;

import net.sqlcipher.database.SQLiteDatabase;

import org.robolectric.annotation.Implementation;
import org.robolectric.annotation.Implements;

/**
 * Created by samuelgithengi on 11/24/20.
 */
@Implements(SQLiteDatabase.class)
public class SQLiteDatabaseShadow {
    @Implementation
    public static synchronized void loadLibs(Context context) {
        //do nothing
    }
}
