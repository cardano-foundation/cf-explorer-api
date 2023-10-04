package org.cardanofoundation.explorer.api.interceptor.auth;

import java.util.List;
import java.util.Set;

import lombok.Getter;
import lombok.Setter;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

@Getter
@Setter
public class RoleConfigurationMapper {

  @JsonProperty("name")
  private String name;
  @JsonProperty("exclude")
  private Set<String> exclude;
  @JsonProperty("function")
  private List<RoleFunction> function;

  @JsonCreator
  public RoleConfigurationMapper(@JsonProperty("name") String name,
                                 @JsonProperty("exclude") Set<String> exclude,
                                 @JsonProperty("function") List<RoleFunction> function) {
    this.name = name;
    this.exclude = exclude;
    this.function = function;
  }
}
