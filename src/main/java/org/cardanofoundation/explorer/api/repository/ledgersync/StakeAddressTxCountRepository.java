package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.springframework.data.jpa.repository.JpaRepository;

import org.cardanofoundation.explorer.common.entity.ledgersync.StakeAddressTxCount;

public interface StakeAddressTxCountRepository extends JpaRepository<StakeAddressTxCount, String> {}
