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
    //RWANDA indicators
    private int healthEducatedChildren5To15;
    private int healthEducatedChildrenAbove16;
    private int vitaminTreatedChildren6To11Months;
    private int vitaminTreatedChildren12To59Months;
    private int albMebTreatedChildren12To59Months;
    private int albMebTreatedChildren5To15Years;
    private int albMebTreatedChildrenAbove16Years;
    private int pzqTreatedChildren5To15Years;
    private int pzqTreatedChildrenAbove16Years;

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

    public int getHealthEducatedChildren5To15() {
        return healthEducatedChildren5To15;
    }

    public void setHealthEducatedChildren5To15(int healthEducatedChildren5To15) {
        this.healthEducatedChildren5To15 = healthEducatedChildren5To15;
    }

    public int getHealthEducatedChildrenAbove16() {
        return healthEducatedChildrenAbove16;
    }

    public void setHealthEducatedChildrenAbove16(int healthEducatedChildrenAbove16) {
        this.healthEducatedChildrenAbove16 = healthEducatedChildrenAbove16;
    }

    public int getVitaminTreatedChildren6To11Months() {
        return vitaminTreatedChildren6To11Months;
    }

    public void setVitaminTreatedChildren6To11Months(int vitaminTreatedChildren6To11Months) {
        this.vitaminTreatedChildren6To11Months = vitaminTreatedChildren6To11Months;
    }

    public int getVitaminTreatedChildren12To59Months() {
        return vitaminTreatedChildren12To59Months;
    }

    public void setVitaminTreatedChildren12To59Months(int vitaminTreatedChildren12To59Months) {
        this.vitaminTreatedChildren12To59Months = vitaminTreatedChildren12To59Months;
    }

    public int getAlbMebTreatedChildren12To59Months() {
        return albMebTreatedChildren12To59Months;
    }

    public void setAlbMebTreatedChildren12To59Months(int albMebTreatedChildren12To59Months) {
        this.albMebTreatedChildren12To59Months = albMebTreatedChildren12To59Months;
    }

    public int getAlbMebTreatedChildren5To15Years() {
        return albMebTreatedChildren5To15Years;
    }

    public void setAlbMebTreatedChildren5To15Years(int albMebTreatedChildren5To15Years) {
        this.albMebTreatedChildren5To15Years = albMebTreatedChildren5To15Years;
    }

    public int getAlbMebTreatedChildrenAbove16Years() {
        return albMebTreatedChildrenAbove16Years;
    }

    public void setAlbMebTreatedChildrenAbove16Years(int albMebTreatedChildrenAbove16Years) {
        this.albMebTreatedChildrenAbove16Years = albMebTreatedChildrenAbove16Years;
    }

    public int getPzqTreatedChildren5To15Years() {
        return pzqTreatedChildren5To15Years;
    }

    public void setPzqTreatedChildren5To15Years(int pzqTreatedChildren5To15Years) {
        this.pzqTreatedChildren5To15Years = pzqTreatedChildren5To15Years;
    }

    public int getPzqTreatedChildrenAbove16Years() {
        return pzqTreatedChildrenAbove16Years;
    }

    public void setPzqTreatedChildrenAbove16Years(int pzqTreatedChildrenAbove16Years) {
        this.pzqTreatedChildrenAbove16Years = pzqTreatedChildrenAbove16Years;
    }
}
