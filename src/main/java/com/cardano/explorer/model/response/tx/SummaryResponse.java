package com.cardano.explorer.model.response.tx;

import com.cardano.explorer.model.response.TxOutResponse;
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
public class SummaryResponse {
  private List<TxOutResponse> stakeAddressTxInputs;
  private List<TxOutResponse> stakeAddressTxOutputs;
}
