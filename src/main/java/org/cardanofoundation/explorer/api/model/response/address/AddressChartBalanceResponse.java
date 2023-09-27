package org.cardanofoundation.explorer.api.model.response.address;

import lombok.*;

import java.math.BigInteger;
import java.util.List;

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
