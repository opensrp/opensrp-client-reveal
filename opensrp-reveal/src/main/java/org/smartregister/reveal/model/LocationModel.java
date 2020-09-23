package org.smartregister.reveal.model;

/**
 * Created by Richard Kareko on 9/22/20.
 */

public class LocationModel {

    private boolean checked;
    private String name;
    public boolean isChecked() {
        return checked;
    }
    public void setChecked(boolean checked) {
        this.checked = checked;
    }
    public String getName() {
        return name;
    }
    public void setName(String name) {
        this.name = name;
    }
    public LocationModel(){
        checked = false;
        name = "";
    }
    public LocationModel(String name){
        checked = false;
        this.name = name;
    }

}
