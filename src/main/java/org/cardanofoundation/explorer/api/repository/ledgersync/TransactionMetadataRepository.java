package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;
import java.util.UUID;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.common.entity.ledgersync.TransactionMetadata;

public interface TransactionMetadataRepository extends JpaRepository<TransactionMetadata, UUID> {
  List<TransactionMetadata> findAllByTxHashOrderByLabelAsc(String txHash);

  List<TransactionMetadata> findAllByTxHash(@Param("txHash") String txHash);
}
