package org.smartregister.reveal.model;

import java.io.Serializable;
import java.util.Map;
import java.util.Set;

/**
 * Created by samuelgithengi on 12/17/19.
 */
public class TaskFilterParams implements Serializable {

    private String sortBy;

    private String searchPhrase;

    private Map<String, Set<String>> checkedFilters;

    public TaskFilterParams(String sortBy, Map<String, Set<String>> checkedFilters) {
        this.sortBy = sortBy;
        this.checkedFilters = checkedFilters;
    }

    public TaskFilterParams(String searchPhrase) {
        setSearchPhrase(searchPhrase);
    }

    public String getSortBy() {
        return sortBy;
    }

    public Map<String, Set<String>> getCheckedFilters() {
        return checkedFilters;
    }

    public String getSearchPhrase() {
        return searchPhrase;
    }

    public void setSearchPhrase(String searchPhrase) {
        this.searchPhrase = searchPhrase;
    }
}
