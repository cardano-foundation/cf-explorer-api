package org.cardanofoundation.explorer.api.repository.aggregation;

import org.cardanofoundation.explorer.consumercommon.entity.aggregation.AggregateAddressToken;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.math.BigInteger;
import java.time.LocalDate;

public interface AggregateAddressTokenRepository extends JpaRepository<AggregateAddressToken, Long> {

  @Query(value = "SELECT sum(aggAddr.balance) "
      + " FROM AggregateAddressToken aggAddr "
      + " WHERE aggAddr.ident = :multiAsset "
      + " AND aggAddr.day > :from and aggAddr.day <= :to ")
  BigInteger sumBalanceInTimeRange(@Param("multiAsset") Long multiAssetId,
                                   @Param("from") LocalDate from,
                                   @Param("to") LocalDate to);
}
