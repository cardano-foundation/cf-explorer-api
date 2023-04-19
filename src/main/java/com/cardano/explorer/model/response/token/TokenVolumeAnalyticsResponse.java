package com.cardano.explorer.model.response.token;

import java.math.BigInteger;
import java.time.LocalDate;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TokenVolumeAnalyticsResponse {
  private LocalDate date;
  private BigInteger value;
}
