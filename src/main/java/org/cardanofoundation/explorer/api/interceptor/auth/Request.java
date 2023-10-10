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

  public Request(@JsonProperty("uri") String uri,@JsonProperty("method") String method) {
    this.uri = uri;
    this.method = method;
  }
}
