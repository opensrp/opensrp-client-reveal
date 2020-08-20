package org.smartregister.reveal.model;

import java.io.Serializable;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import lombok.Builder;
import lombok.Data;

/**
 * Created by samuelgithengi on 12/17/19.
 */
@Data
@Builder
public class TaskFilterParams implements Serializable {

    private String sortBy;

    private String searchPhrase;

    private Date fromDate;

    private boolean viewAllEvents;

    @Builder.Default
    private Map<String, Set<String>> checkedFilters = new HashMap<>();


}
