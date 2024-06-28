package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.springframework.data.jpa.repository.JpaRepository;

import org.cardanofoundation.explorer.common.entity.ledgersync.TokenTxCount;

public interface TokenTxCountRepository extends JpaRepository<TokenTxCount, String> {}
