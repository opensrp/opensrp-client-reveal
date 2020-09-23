package org.smartregister.reveal.model;

/**
 * Created by Richard Kareko on 9/22/20.
 */

public class LocationModel {

    private boolean checked;
    private String name;
    private String id;
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
    public String getId() {
        return id;
    }
    public void setId(String id) {
        this.id = id;
    }

    public LocationModel(){
        checked = false;
        name = "";
    }

}
