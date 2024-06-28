package org.cardanofoundation.explorer.api.repository.ledgersyncagg;

import java.time.LocalDate;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.AggregateAddressToken;

public interface AggregateAddressTokenRepository
    extends JpaRepository<AggregateAddressToken, Long> {

  List<AggregateAddressToken> findAllByUnitAndDayBetween(String unit, LocalDate from, LocalDate to);
}
