package org.smartregister.reveal.model;

import java.util.List;

/**
 * Created by ndegwamartin on 2019-09-25.
 */
public class IndicatorDetails {

    private int sprayed;
    private int notSprayed;
    private int totalStructures;
    private int progress;
    private int notVisited;
    private int ineligible;
    private int foundStructures;
    private int roomCoverage;
    private List<String> sprayIndicatorList;
    private int target;
    private int completeDrugDistribution;
    private int partialDrugDistribution;
    private int totalIndividualTreated;
    private int childrenEligible;

    public int getSprayed() {
        return sprayed;
    }

    public void setSprayed(int sprayed) {
        this.sprayed = sprayed;
    }

    public int getNotSprayed() {
        return notSprayed;
    }

    public void setNotSprayed(int notSprayed) {
        this.notSprayed = notSprayed;
    }

    public int getTotalStructures() {
        return totalStructures;
    }

    public void setTotalStructures(int totalStructures) {
        this.totalStructures = totalStructures;
    }

    public int getProgress() {
        return progress;
    }

    public void setProgress(int progress) {
        this.progress = progress;
    }

    public int getNotVisited() {
        return notVisited;
    }

    public void setNotVisited(int notVisited) {
        this.notVisited = notVisited;
    }

    public List<String> getSprayIndicatorList() {
        return sprayIndicatorList;
    }

    public void setSprayIndicatorList(List<String> sprayIndicatorList) {
        this.sprayIndicatorList = sprayIndicatorList;
    }

    public int getIneligible() {
        return ineligible;
    }

    public void setIneligible(int ineligible) {
        this.ineligible = ineligible;
    }

    public int getFoundStructures() {
        return foundStructures;
    }

    public void setFoundStructures(int foundStructures) {
        this.foundStructures = foundStructures;
    }

    public int getRoomCoverage() {
        return roomCoverage;
    }

    public int getTarget() {
        return target;
    }

    public void setTarget(int target) {
        this.target = target;
    }

    public void setRoomCoverage(int roomCoverage) {
        this.roomCoverage = roomCoverage;
    }

    public int getCompleteDrugDistribution() {
        return completeDrugDistribution;
    }

    public void setCompleteDrugDistribution(int completeDrugDistribution) {
        this.completeDrugDistribution = completeDrugDistribution;
    }

    public int getPartialDrugDistribution() {
        return partialDrugDistribution;
    }

    public void setPartialDrugDistribution(int partialDrugDistribution) {
        this.partialDrugDistribution = partialDrugDistribution;
    }

    public int getTotalIndividualTreated() {
        return totalIndividualTreated;
    }

    public void setTotalIndividualTreated(int totalIndividualTreated) {
        this.totalIndividualTreated = totalIndividualTreated;
    }

    public int getChildrenEligible() {
        return childrenEligible;
    }

    public void setChildrenEligible(int childrenEligible) {
        this.childrenEligible = childrenEligible;
    }
}
