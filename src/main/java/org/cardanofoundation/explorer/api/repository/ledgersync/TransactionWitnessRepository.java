package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;

import io.lettuce.core.dynamic.annotation.Param;

import org.cardanofoundation.explorer.common.entity.ledgersync.TransactionWitness;
import org.cardanofoundation.explorer.common.entity.ledgersync.TransactionWitnessId;

public interface TransactionWitnessRepository
    extends JpaRepository<TransactionWitness, TransactionWitnessId> {
  List<TransactionWitness> findAllByTxHash(@Param("tx_hash") String txHash);
}
