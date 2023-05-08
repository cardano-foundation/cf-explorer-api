package org.cardanofoundation.explorer.api.model.response.pool.projection;

import java.math.BigInteger;
import java.util.Date;

public interface PoolReportProjection {
    Integer getEpochNo();

    BigInteger  getSize();

    String getTxnHash();

    Date getTimestamp();

    BigInteger getAdaValueHold();

    BigInteger getAdaValueFees();

    String getOwner();

    BigInteger getOperatorReward();

    String getRewardAccount();
}
