package org.cardanofoundation.explorer.api.model.response;

import java.time.LocalDate;

import lombok.*;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class BlockPropagationResponse {

  @JsonProperty("epochNo")
  private Integer epochNo;

  @JsonProperty("time")
  @JsonFormat(pattern = "yyyy-MM-dd")
  private LocalDate time;

  @JsonProperty("blockPropMean")
  private Integer blockPropMean;

  @JsonProperty("blockPropMedian")
  private Integer blockPropMedian;

  @JsonProperty("blockPropP90")
  private Integer blockPropP90;

  @JsonProperty("blockPropP95")
  private Integer blockPropP95;
}
