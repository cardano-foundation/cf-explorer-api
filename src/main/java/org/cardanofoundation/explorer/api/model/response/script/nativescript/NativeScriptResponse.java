package org.cardanofoundation.explorer.api.model.response.script.nativescript;

import java.math.BigInteger;
import java.time.LocalDateTime;
import java.util.List;

import lombok.*;

import org.cardanofoundation.ledgersync.common.common.nativescript.ScriptType;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class NativeScriptResponse {
  private String scriptHash;
  private Long numberOfTokens;
  private Long numberOfAssetHolders;
  private ScriptType conditionType;
  private BigInteger required;
  private List<String> keyHashes;
  private LocalDateTime after;
  private LocalDateTime before;
  private List<String> associatedAddress;
  private String script;
  private Boolean isOneTimeMint;
  private Boolean verifiedContract;
}
