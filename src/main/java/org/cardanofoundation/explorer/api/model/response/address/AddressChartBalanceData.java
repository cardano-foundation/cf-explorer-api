package org.cardanofoundation.explorer.api.model.response.address;

import java.math.BigInteger;
import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AddressChartBalanceData {
  private LocalDateTime date;
  private BigInteger value;
}
