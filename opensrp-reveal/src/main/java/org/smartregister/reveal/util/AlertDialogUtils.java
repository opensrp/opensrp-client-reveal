package org.smartregister.reveal.util;

import android.content.Context;
import android.content.DialogInterface;
import android.support.v7.app.AlertDialog;

import org.smartregister.reveal.R;

/**
 * Created by samuelgithengi on 3/22/19.
 */
public class AlertDialogUtils {


    public static AlertDialog displayNotification(Context context, int title, int message, Object... formatArgs) {
        if (formatArgs.length == 0)
            return new AlertDialog.Builder(context).setMessage(message).setTitle(title).setPositiveButton(R.string.ok, null).show();
        else
            return new AlertDialog.Builder(context).setMessage(context.getString(message, formatArgs)).setTitle(title).setPositiveButton(R.string.ok, null).show();
    }


    public static AlertDialog displayNotification(Context context, String message) {
        return new AlertDialog.Builder(context).setMessage(message).setTitle(R.string.fetch_structures_title).setPositiveButton(R.string.ok, null).show();
    }

    public static AlertDialog displayNotificationWithCallback(Context context, int title, int message, int positiveBtnTitle, int negativeBtnTitle, DialogInterface.OnClickListener onClickListener, Object... formatArgs) {
        AlertDialog alert = null;

        if (formatArgs.length == 0)
            alert = new AlertDialog.Builder(context).setMessage(message).setTitle(title).setPositiveButton(positiveBtnTitle, onClickListener).setNegativeButton(negativeBtnTitle,onClickListener).show();
        else
            alert = new AlertDialog.Builder(context).setMessage(context.getString(message, formatArgs)).setTitle(title).setPositiveButton(positiveBtnTitle, onClickListener).setNegativeButton(negativeBtnTitle,onClickListener).show();

        return alert;
    }
}
