package org.smartregister.reveal.model;

import androidx.annotation.ArrayRes;

import java.io.Serializable;
import java.util.List;

import lombok.Builder;
import lombok.Data;

/**
 * Created by samuelgithengi on 8/10/20.
 */
@Data
@Builder

public class FilterConfiguration implements Serializable {

    @Builder.Default
    private boolean businessStatusLayoutEnabled = true;

    @Builder.Default
    private boolean taskCodeLayoutEnabled = true;

    @Builder.Default
    private boolean interventionTypeLayoutEnabled = true;

    private boolean formsLayoutEnabled;

    private boolean filterFromDateAndViewAllEventsEnabled;

    private List<String> businessStatusList;

    private List<String> eventTypeList;

    @ArrayRes
    private Integer sortOptions;
}
