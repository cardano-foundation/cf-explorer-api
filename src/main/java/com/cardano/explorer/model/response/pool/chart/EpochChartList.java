package com.cardano.explorer.model.response.pool.chart;

import com.cardano.explorer.model.response.pool.projection.EpochChartProjection;
import java.math.BigInteger;
import lombok.Getter;
import lombok.Setter;


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
