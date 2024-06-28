package org.cardanofoundation.explorer.api.repository.ledgersyncagg;

import org.springframework.data.jpa.repository.JpaRepository;

import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.StakeAddressTxCount;

public interface StakeAddressTxCountRepository extends JpaRepository<StakeAddressTxCount, String> {}
