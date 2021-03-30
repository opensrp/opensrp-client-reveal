package org.smartregister.reveal.util;

import com.vijay.jsonwizard.utils.JsonFormMLSAssetGenerator;

import org.junit.Test;

public class GenerateJsonFormMLSAssetTest {


    @Test
    public void generateMLSThailand() throws Exception {
        String formToTranslate = "/home/ona/kitchen/src/github.com/OpenSRP/opensrp-client-reveal/opensrp-reveal/src/main/assets/json.form/thailand_mosquito_collection_form.json";
        System.out.println("Injecting placeholders in form at path: " + formToTranslate + " ...\n");
        JsonFormMLSAssetGenerator.processForm(formToTranslate);
    }

}
