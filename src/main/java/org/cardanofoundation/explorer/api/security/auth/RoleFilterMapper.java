package org.cardanofoundation.explorer.api.security.auth;

import java.util.List;

import lombok.Builder;
import lombok.Getter;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

@Getter
@Builder
@JsonInclude(JsonInclude.Include.NON_NULL)
public class RoleFilterMapper {

  @JsonProperty("role")
  private List<RoleConfigurationMapper> roles;
  @JsonProperty("auth")
  private List<Request> auth;

  @JsonCreator
  public RoleFilterMapper(
      @JsonProperty("role") List<RoleConfigurationMapper> roles,
      @JsonProperty("auth") List<Request> auth) {
    this.roles = roles;
    this.auth = auth;
  }
}

