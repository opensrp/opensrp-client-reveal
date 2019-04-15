package org.smartregister.reveal.contract;

/**
 * Created by samuelgithengi on 4/14/19.
 */
public interface FamilyRegisterContract extends org.smartregister.family.contract.FamilyRegisterContract {

    interface View extends org.smartregister.family.contract.FamilyRegisterContract.View {

        void startProfileActivity(String baseEntityId, String familyHead, String primaryCareGiver,
                                  String cityVillage, String firstName);
    }
}
