package org.cardanofoundation.explorer.api.model.request.script.nativescript;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class NativeScriptFilterRequest {
  private Boolean openTimeLocked;
  private Boolean isMultiSig;
}
