package org.smartregister.reveal.interactor;

import org.smartregister.reveal.application.RevealApplication;
import org.smartregister.reveal.sync.RevealClientProcessor;
import org.smartregister.sync.ClientProcessorForJava;

/**
 * Created by samuelgithengi on 4/15/19.
 */
public class RevealFamilyRegisterInteractor extends org.smartregister.family.interactor.FamilyRegisterInteractor {

    @Override
    public ClientProcessorForJava getClientProcessorForJava() {
        return RevealClientProcessor.getInstance(RevealApplication.getInstance().getApplicationContext());
    }
}
