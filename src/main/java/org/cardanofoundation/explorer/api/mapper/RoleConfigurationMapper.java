package org.cardanofoundation.explorer.api.mapper;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.Getter;
import lombok.Setter;

import java.util.List;
import java.util.Set;

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
