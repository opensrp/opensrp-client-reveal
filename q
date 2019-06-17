[1mdiff --git a/opensrp-reveal/src/test/java/org/smartregister/reveal/presenter/FamilyRegisterPresenterTest.java b/opensrp-reveal/src/test/java/org/smartregister/reveal/presenter/FamilyRegisterPresenterTest.java[m
[1mindex 2bceedc..17ce094 100644[m
[1m--- a/opensrp-reveal/src/test/java/org/smartregister/reveal/presenter/FamilyRegisterPresenterTest.java[m
[1m+++ b/opensrp-reveal/src/test/java/org/smartregister/reveal/presenter/FamilyRegisterPresenterTest.java[m
[36m@@ -98,7 +98,7 @@[m [mpublic class FamilyRegisterPresenterTest extends BaseUnitTest {[m
     public void testOnTasksGenerated() {[m
         presenter.onTasksGenerated();[m
         verify(view).hideProgressDialog();[m
[31m-        verify(view).startProfileActivity(baseEntityId, familyHead, careGiver, "vl1", "Otala");[m
[32m+[m[32m        verify(view).startProfileActivity(baseEntityId, familyHead, careGiver, "Otala");[m
         assertTrue(RevealApplication.getInstance().isFamilyAdded());[m
     }[m
 }[m
