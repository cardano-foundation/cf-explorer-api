package org.cardanofoundation.explorer.api.model.response.pool.chart;

import java.math.BigInteger;
import java.util.List;

public class EpochChartResponse extends BasePoolChart<EpochChartList, BigInteger> {

  public EpochChartResponse() {
    this.setDataByDays(List.of());
    this.setLowest(BigInteger.ZERO);
    this.setHighest(BigInteger.ZERO);
  }
}
