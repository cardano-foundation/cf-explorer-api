package org.cardanofoundation.explorer.api.model.response.tx;

import java.util.List;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CollateralResponse {
  private List<TxOutResponse> collateralInputResponses;
  private List<TxOutResponse> collateralOutputResponses;
}
