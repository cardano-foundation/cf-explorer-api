package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.cardanofoundation.explorer.consumercommon.entity.TxBootstrapWitnesses;

public interface TxBootstrapWitnessesRepository extends JpaRepository<TxBootstrapWitnesses, Long> {

  List<TxBootstrapWitnesses> findAllByTx(@Param("tx") Tx tx);
}
