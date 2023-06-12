package org.cardanofoundation.explorer.api.model.response.token;

import java.math.BigInteger;
import java.time.LocalDate;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class TokenVolumeAnalyticsResponse {
  private LocalDate date;
  private BigInteger value;

  public TokenVolumeAnalyticsResponse(LocalDate date, BigInteger value) {
    this.date = date;
    this.value = value == null ? BigInteger.ZERO : value;
  }
}
