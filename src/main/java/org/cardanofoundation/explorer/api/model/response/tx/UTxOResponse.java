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
public class UTxOResponse {

  private List<TxOutResponse> inputs;
  private List<TxOutResponse> outputs;
}
