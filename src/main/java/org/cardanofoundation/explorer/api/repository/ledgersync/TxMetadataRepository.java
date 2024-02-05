package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.cardanofoundation.explorer.consumercommon.entity.TxMetadata;

public interface TxMetadataRepository extends JpaRepository<TxMetadata, Long> {

  List<TxMetadata> findAllByTxOrderByKeyAsc(Tx tx);

  @Query("SELECT t FROM TxMetadata t WHERE t.tx.hash = :txHash")
  List<TxMetadata> findAllByTxHash(@Param("txHash") String txHash);
}
