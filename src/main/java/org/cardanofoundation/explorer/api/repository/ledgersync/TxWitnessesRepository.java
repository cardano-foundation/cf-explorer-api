package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.common.entity.ledgersync.Tx;
import org.cardanofoundation.explorer.common.entity.ledgersync.TxWitness;

public interface TxWitnessesRepository extends JpaRepository<TxWitness, Long> {

  List<TxWitness> findAllByTx(@Param("pub_key") String pubKey);
}
