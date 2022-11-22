package com.cardano.explorer.model.response.tx;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class CollateralResponse {
  private CollateralInputResponse collateralInput;
  private CollateralOutputResponse collateralOutput;
}
