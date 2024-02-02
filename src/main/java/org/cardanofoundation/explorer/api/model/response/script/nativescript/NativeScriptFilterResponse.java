package org.cardanofoundation.explorer.api.model.response.script.nativescript;

import lombok.*;
import org.cardanofoundation.explorer.api.model.response.token.TokenFilterResponse;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

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
  private Boolean isOpen = false;
  private Long numberOfTokens = 0L;
  private Long numberOfAssetHolders = 0L;
  private List<TokenFilterResponse> tokens = new ArrayList<>();
}
