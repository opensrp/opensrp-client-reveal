package org.smartregister.reveal.model;

import android.content.Context;
import android.support.annotation.NonNull;

import org.apache.commons.lang3.StringUtils;
import org.smartregister.view.ListContract;

import java.util.Date;

public class Child implements ListContract.Identifiable {

    private String baseEntityID;
    private Date birthDate;
    private String firstName;
    private String lastName;
    private String middleName;
    private String gender;
    private String uniqueID;
    private String grade;

    public String getBaseEntityID() {
        return baseEntityID;
    }

    public void setBaseEntityID(String baseEntityID) {
        this.baseEntityID = baseEntityID;
    }

    public Date getBirthDate() {
        return birthDate;
    }

    public void setBirthDate(Date birthDate) {
        this.birthDate = birthDate;
    }

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    public String getMiddleName() {
        return middleName;
    }

    public void setMiddleName(String middleName) {
        this.middleName = middleName;
    }

    public String getGender() {
        return gender;
    }

    public void setGender(String gender) {
        this.gender = gender;
    }

    public String getUniqueID() {
        return uniqueID;
    }

    public void setUniqueID(String uniqueID) {
        this.uniqueID = uniqueID;
    }

    public String getGrade() {
        return grade;
    }

    public void setGrade(String grade) {
        this.grade = grade;
    }

    public String getFullName() {
        return (StringUtils.trim(StringUtils.capitalize(firstName)) + " " + StringUtils.trim(StringUtils.capitalize(lastName))).trim();
    }

    /**
     * Returns a context aware age String
     *
     * @param context
     * @return
     */
    public String getAge(Context context) {
        return null;
    }

    @NonNull
    @Override
    public String getID() {
        return baseEntityID;
    }
}
