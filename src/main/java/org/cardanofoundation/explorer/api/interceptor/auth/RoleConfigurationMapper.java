package org.cardanofoundation.explorer.api.interceptor.auth;

import java.util.List;
import java.util.Map;

import lombok.Getter;
import lombok.Setter;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

@Getter
@Setter
public class RoleConfigurationMapper {

  @JsonProperty("name")
  private String name;

  @JsonProperty("attributes")
  private Map<String,Object> attributes;

  @JsonProperty("function")
  private List<RoleFunction> function;

  @JsonCreator
  public RoleConfigurationMapper(@JsonProperty("name") String name,
                                 @JsonProperty("attributes") Map<String,Object> attributes,
                                 @JsonProperty("function") List<RoleFunction> function) {
    this.name = name;
    this.attributes = attributes;
    this.function = function;
  }
}
