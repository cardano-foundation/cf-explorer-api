package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.common.entity.ledgersync.TransactionMetadata;

public interface TxMetadataRepository extends JpaRepository<TransactionMetadata, Long> {

  List<TransactionMetadata> findAllByTxHashOrderByLabelAsc(String txHash);

  List<TransactionMetadata> findAllByTxHash(@Param("txHash") String txHash);
}
