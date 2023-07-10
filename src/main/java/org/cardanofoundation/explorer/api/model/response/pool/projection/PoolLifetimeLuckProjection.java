package org.cardanofoundation.explorer.api.model.response.pool.projection;

import java.math.BigDecimal;
import java.math.BigInteger;

public interface PoolLifetimeLuckProjection {
    Integer getEpochNo();

    BigInteger getBlkProducedByPool();

    BigInteger getBlkProducedByAllPool();

    BigDecimal getDecentralisation();
}
