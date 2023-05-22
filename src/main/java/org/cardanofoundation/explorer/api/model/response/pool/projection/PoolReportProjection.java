package org.cardanofoundation.explorer.api.model.response.pool.projection;

import java.math.BigInteger;
import java.util.Date;

public interface PoolReportProjection {
    Integer getEpochNo();

    BigInteger  getSize();

    Date getTimestamp();

    BigInteger getFee();
}
