package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;

import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import org.cardanofoundation.explorer.common.entity.compositeKey.AddressBalanceId;
import org.cardanofoundation.explorer.common.entity.ledgersync.LatestAddressBalance;

public interface LatestAddressBalanceRepository
    extends JpaRepository<LatestAddressBalance, AddressBalanceId> {

  @Query(value = "SELECT lab FROM LatestAddressBalance lab")
  List<LatestAddressBalance> findAllLatestAddressBalance(Pageable pageable);
}
