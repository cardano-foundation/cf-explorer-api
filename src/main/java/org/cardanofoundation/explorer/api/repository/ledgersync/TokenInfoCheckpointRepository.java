package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.consumercommon.entity.TokenInfoCheckpoint;

@Repository
public interface TokenInfoCheckpointRepository extends JpaRepository<TokenInfoCheckpoint, Long> {
}
