package org.cardanofoundation.explorer.api.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.consumercommon.entity.TokenInfoCheckpoint;

@Repository
public interface TokenInfoCheckpointRepository extends JpaRepository<TokenInfoCheckpoint, Long> {
  @Query("select max(blockNo) from TokenInfoCheckpoint")
  Optional<Long> findMaxBlockNoCheckpoint();
}
