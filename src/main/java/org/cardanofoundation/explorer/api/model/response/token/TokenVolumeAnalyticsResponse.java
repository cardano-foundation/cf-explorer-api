package org.cardanofoundation.explorer.api.model.response.token;

import java.math.BigInteger;
import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class TokenVolumeAnalyticsResponse {
  private LocalDateTime date;
  private BigInteger value;
}
