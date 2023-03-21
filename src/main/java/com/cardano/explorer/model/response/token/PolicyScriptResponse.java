package com.cardano.explorer.model.response.token;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class PolicyScriptResponse {
  private String script;
  private String byteCode;
}