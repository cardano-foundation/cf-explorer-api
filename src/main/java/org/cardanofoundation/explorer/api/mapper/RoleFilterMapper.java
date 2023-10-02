package org.cardanofoundation.explorer.api.mapper;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

import java.util.List;
import java.util.Set;

@Getter
@Setter
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class RoleFilterMapper {

  @JsonProperty("auth")
  private Set<String> auth;
  @JsonProperty("role")
  private List<RoleConfigurationMapper> roles;

  @JsonCreator
  public RoleFilterMapper(@JsonProperty("auth") Set<String> auth,
                          @JsonProperty("role") List<RoleConfigurationMapper> roles) {
    this.auth = auth;
    this.roles = roles;
  }
}

