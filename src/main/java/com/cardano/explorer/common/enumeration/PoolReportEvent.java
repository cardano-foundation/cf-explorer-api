package com.cardano.explorer.common.enumeration;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.experimental.FieldDefaults;

@FieldDefaults(level = AccessLevel.PRIVATE, makeFinal = true)
@RequiredArgsConstructor
@Getter
public enum PoolReportEvent {
    @JsonProperty("all")
    ALL("all"),

    @JsonProperty("registration")
    REGISTRATION("registration"),

    @JsonProperty("deregistration")
    DEREGISTRATION("deregistration"),

    @JsonProperty("reward")
    REWARD("reward"),

    @JsonProperty("pool_update")
    POOL_UPDATE("pool_update");

    String value;

}
