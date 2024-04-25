package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.time.LocalDate;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import org.cardanofoundation.explorer.common.entity.ledgersync.aggregation.AggregateAddressToken;

public interface AggregateAddressTokenRepository
    extends JpaRepository<AggregateAddressToken, Long> {

  List<AggregateAddressToken> findAllByIdentAndDayBetween(
      Long multiAssetId, LocalDate from, LocalDate to);
}
