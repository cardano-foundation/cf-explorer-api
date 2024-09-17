package org.cardanofoundation.explorer.api.repository.ledgersyncagg;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.StakeAddressTxCount;

public interface StakeAddressTxCountRepository extends JpaRepository<StakeAddressTxCount, String> {
  Optional<StakeAddressTxCount> findByStakeAddress(String stakeAddress);
}
