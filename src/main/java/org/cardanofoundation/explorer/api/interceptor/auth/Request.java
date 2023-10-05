package org.cardanofoundation.explorer.api.interceptor.auth;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Data;
import lombok.experimental.FieldDefaults;

import com.fasterxml.jackson.annotation.JsonProperty;

@Data
@FieldDefaults(level = AccessLevel.PRIVATE)
@Builder
public class Request {
  @JsonProperty("uri")
  String uri;
  @JsonProperty("method")
  String method;
  @JsonProperty("roles")
  String[] roles;

  public Request(@JsonProperty("uri") String uri,@JsonProperty("method") String method, @JsonProperty("roles")
  String[] roles) {
    this.uri = uri;
    this.method = method;
    this.roles = roles;
  }
}
