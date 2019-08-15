package org.smartregister.reveal.util;

import android.content.Context;
import android.support.v7.app.AlertDialog;
import android.view.View;

import org.smartregister.reveal.R;
import org.smartregister.reveal.contract.AlertDialogCallback;

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

    public static AlertDialog displayNotificationWithCallback(Context context, int title, int message, int positiveBtnTitle, int negativeBtnTitle, AlertDialogCallback callback, Object... formatArgs) {
        AlertDialog alert = null;

        if (formatArgs.length == 0)
            alert = new AlertDialog.Builder(context).setMessage(message).setTitle(title).setPositiveButton(positiveBtnTitle, null).setNegativeButton(negativeBtnTitle,null).show();
        else
            alert = new AlertDialog.Builder(context).setMessage(context.getString(message, formatArgs)).setTitle(title).setPositiveButton(positiveBtnTitle, null).setNegativeButton(negativeBtnTitle,null).show();

        alert.getButton(AlertDialog.BUTTON_POSITIVE).setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                callback.onPositiveBtnClicked();
            }
        });

        return alert;
    }
}
