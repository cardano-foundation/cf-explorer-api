package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.cardanofoundation.explorer.consumercommon.entity.TxMetadata;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface TxMetadataRepository extends JpaRepository<TxMetadata, Long> {

  List<TxMetadata> findAllByTxOrderByKeyAsc(Tx tx);
}
