package org.cardanofoundation.explorer.api.mapper;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.Getter;
import lombok.Setter;

import java.util.Map;

@Getter
@Setter
public class RoleFunction {

  @JsonProperty("uri")
  private String uri;
  @JsonProperty("description")
  private Map<String, Object> description;

  @JsonCreator
  public RoleFunction(@JsonProperty("uri") String uri,
                      @JsonProperty("description") Map<String, Object> description) {
    this.uri = uri;
    this.description = description;
  }
}
