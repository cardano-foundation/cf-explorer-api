package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.springframework.data.jpa.repository.JpaRepository;

import org.cardanofoundation.explorer.common.entity.compositeKey.AddressBalanceId;
import org.cardanofoundation.explorer.common.entity.ledgersync.LatestAddressBalance;

public interface LatestAddressBalanceRepository
    extends JpaRepository<LatestAddressBalance, AddressBalanceId> {}
