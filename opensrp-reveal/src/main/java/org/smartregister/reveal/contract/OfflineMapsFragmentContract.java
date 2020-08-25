package org.smartregister.reveal.contract;

public interface OfflineMapsFragmentContract {

    interface View {

        void displayToast(String message);

        void displayError(int title, String message);

    }

}
