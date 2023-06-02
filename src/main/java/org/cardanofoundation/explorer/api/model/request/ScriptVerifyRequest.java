package org.cardanofoundation.explorer.api.model.request;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class ScriptVerifyRequest {

  private String address;
  private String script;
}
