package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.math.BigInteger;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;
import org.cardanofoundation.explorer.consumercommon.entity.aggregation.AggregateAddressToken;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface AggregateAddressTokenRepository extends JpaRepository<AggregateAddressToken, Long> {

  @Query(value = "SELECT sum(aggAddr.balance) "
      + " FROM AggregateAddressToken aggAddr "
      + " WHERE aggAddr.ident = :multiAsset "
      + " AND aggAddr.day > :from and aggAddr.day <= :to ")
  Optional<BigInteger> sumBalanceInTimeRange(@Param("multiAsset") Long multiAssetId,
                                             @Param("from") LocalDate from,
                                             @Param("to") LocalDate to);

  List<AggregateAddressToken> findAllByIdentAndDayBetween(Long multiAssetId, LocalDate from, LocalDate to);

  @Query("select max(a.day) from AggregateAddressToken a")
  Optional<LocalDate> getMaxDay();
}
