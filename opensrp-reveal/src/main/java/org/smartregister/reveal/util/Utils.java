package org.smartregister.reveal.util;

import static org.smartregister.tasking.util.Constants.CONFIGURATION;
import static org.smartregister.tasking.util.Utils.getGlobalConfig;

/**
 * Created by Ephraim Kigamba - nek.eam@gmail.com on 12-08-2020.
 */
public class Utils {


    /**
     * Uses the server setting "draw_operational_area_boundary_and_label" to determine whether to draw the operational area boundary
     * If this variable is not available on the server the DEFAULT_DRAW_OPERATIONAL_AREA_BOUNDARY_AND_LABEL value from the constants file is used
     *
     * @return drawOperationalAreaBoundaryAndLabel
     */
    public static Boolean getDrawOperationalAreaBoundaryAndLabel() {
        return Boolean.valueOf(getGlobalConfig(CONFIGURATION.DRAW_OPERATIONAL_AREA_BOUNDARY_AND_LABEL, CONFIGURATION.DEFAULT_DRAW_OPERATIONAL_AREA_BOUNDARY_AND_LABEL.toString()));
    }

    public static String getAge(String dob) {
        String dobString = org.smartregister.family.util.Utils.getDuration(dob);
        return dobString.contains("y") ? dobString.substring(0, dobString.indexOf("y")) : dobString;
    }


    /**
     * Uses the server setting "display_add_structure_out_of_boundary_warning_dialog" to determine
     * whether to display the "Register structure outside area boundary" warning dialog
     *
     * <p>
     * If this variable is not available on the server the DEFAULT_DRAW_OPERATIONAL_AREA_BOUNDARY_AND_LABEL value from the constants file is used
     *
     * @return displayAddStructureOutOfBoundaryWarningDialog
     */
    public static Boolean displayAddStructureOutOfBoundaryWarningDialog() {
        return Boolean.valueOf(getGlobalConfig(CONFIGURATION.DISPLAY_ADD_STRUCTURE_OUT_OF_BOUNDARY_WARNING_DIALOG, CONFIGURATION.DEFAULT_DISPLAY_ADD_STRUCTURE_OUT_OF_BOUNDARY_WARNING_DIALOG.toString()));
    }


    /**
     * Determines whether a structure is a residence based on the Task Code value
     *
     * @param taskCode
     * @return isResidentialStructure
     */
    /*public static boolean isResidentialStructure(String taskCode) {
        if (StringUtils.isEmpty(taskCode)) {
            return false;
        }
        return !(Constants.Intervention.MOSQUITO_COLLECTION.equals(taskCode)
                || Constants.Intervention.LARVAL_DIPPING.equals(taskCode)
                || Constants.Intervention.PAOT.equals(taskCode));
    }*/

}
