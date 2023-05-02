package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.projection.TxContractProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Redeemer;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface RedeemerRepository extends JpaRepository<Redeemer, Long> {

  @Query("SELECT re.scriptHash AS scriptHash, txOut.address AS address, re.purpose as purpose"
      + " FROM Redeemer re"
      + " INNER JOIN Tx tx ON re.tx = tx"
      + " LEFT JOIN TxIn txIn ON txIn.redeemer = re"
      + " LEFT JOIN TxOut txOut ON txIn.txOut = txOut.tx AND txIn.txOutIndex = txOut.index "
      + " WHERE tx = :tx")
  List<TxContractProjection> findContractByTx(Tx tx);
}
