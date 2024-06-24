package org.cardanofoundation.explorer.api.repository.ledgersyncagg;

import org.springframework.data.jpa.repository.JpaRepository;

import org.cardanofoundation.explorer.common.entity.compositeKey.StakeAddressBalanceId;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.TopStakeAddressBalance;

public interface TopStakeAddressBalanceRepository
    extends JpaRepository<TopStakeAddressBalance, StakeAddressBalanceId> {}
