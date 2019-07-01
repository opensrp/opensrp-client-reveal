package org.smartregister.reveal.contract;

import org.json.JSONObject;

public interface CaseClassificationContract {

    interface View {
        void displayIndexCase(JSONObject indexCase);
    }
}
