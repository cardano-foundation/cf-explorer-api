package org.cardanofoundation.explorer.api.model.response.address;

import java.math.BigInteger;
import java.util.List;

import lombok.*;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AddressChartBalanceResponse {
  private List<AddressChartBalanceData> data;
  private BigInteger highestBalance;
  private BigInteger lowestBalance;
}
