package org.cardanofoundation.explorer.api.mapper;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class RoleFilterMapper {
    private String role;
    private String apiUrl;
    private Integer count;
}