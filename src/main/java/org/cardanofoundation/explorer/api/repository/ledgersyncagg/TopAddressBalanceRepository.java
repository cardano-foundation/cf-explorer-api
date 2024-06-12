package org.cardanofoundation.explorer.api.repository.ledgersyncagg;

import java.util.List;

import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.TopAddressBalance;

public interface TopAddressBalanceRepository
    extends JpaRepository<TopAddressBalance, String> {

  @Query(value = "SELECT tab FROM TopAddressBalance tab")
  List<TopAddressBalance> findAllLatestAddressBalance(Pageable pageable);
}
