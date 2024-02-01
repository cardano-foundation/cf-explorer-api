package org.cardanofoundation.explorer.api.model.response.script.nativescript;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

import lombok.*;

import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class NativeScriptFilterResponse {
  private String scriptHash;
  private LocalDateTime after;
  private LocalDateTime before;
  private Boolean isMultiSig = false;
  private Long numberOfTokens = 0L;
  private Long numberOfAssetHolders = 0L;
  private List<TokenFilterResponse> tokens = new ArrayList<>();
}
