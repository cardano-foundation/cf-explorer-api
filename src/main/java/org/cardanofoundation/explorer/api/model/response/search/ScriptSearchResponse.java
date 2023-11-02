package org.cardanofoundation.explorer.api.model.response.search;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class ScriptSearchResponse {
  private String scriptHash;
  private boolean isNativeScript = false;
  private boolean isSmartContract = false;
}
