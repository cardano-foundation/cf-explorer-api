package org.cardanofoundation.explorer.api.model.response.script.nativescript;

import lombok.*;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class NativeScriptFilterResponse {
  private String scriptHash;
  private Integer numberOfTokens;
  private Integer numberOfAssetHolders;
}
