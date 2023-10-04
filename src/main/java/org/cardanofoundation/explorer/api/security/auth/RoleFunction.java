package org.cardanofoundation.explorer.api.security.auth;

import java.util.Map;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

@Getter
@Setter
@NoArgsConstructor
public class RoleFunction {

  @JsonProperty("uri")
  private String uri;
  @JsonProperty("method")
  private String method;
  @JsonProperty("description")
  private Map<String, Object> description;


  @JsonCreator
  public RoleFunction(@JsonProperty("uri") String uri,
                      @JsonProperty("method") String method,
                      @JsonProperty("description") Map<String, Object> description) {
    this.uri = uri;
    this.method = method;
    this.description = description;
  }
}
