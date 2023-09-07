package org.cardanofoundation.explorer.api.model.response.dashboard;

import com.fasterxml.jackson.databind.annotation.JsonSerialize;
import java.math.BigInteger;
import java.time.LocalDateTime;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.cardanofoundation.explorer.api.config.JackSonDateTimeSerializer;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class EpochSummary {
  protected Integer no;
  private Integer slot;
  private Integer totalSlot;
  private Integer account;

  @JsonSerialize(using = JackSonDateTimeSerializer.class)
  private LocalDateTime startTime;

  @JsonSerialize(using = JackSonDateTimeSerializer.class)
  private LocalDateTime endTime;

  private BigInteger circulatingSupply;
}
