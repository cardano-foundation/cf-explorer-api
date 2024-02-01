package org.cardanofoundation.explorer.api.model.response.pool.chart;

import java.math.BigInteger;

import lombok.Getter;
import lombok.Setter;

import org.cardanofoundation.explorer.api.model.response.pool.projection.EpochChartProjection;

@Getter
@Setter
public class EpochChartList {

  private Integer epochNo;

  private BigInteger totalStake;

  public EpochChartList(EpochChartProjection epochChart) {
    this.epochNo = epochChart.getChartKey();
    this.totalStake = epochChart.getChartValue();
  }
}
