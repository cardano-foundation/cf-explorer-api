package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.projection.TxContractProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Redeemer;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface RedeemerRepository extends JpaRepository<Redeemer, Long> {

  @Query("SELECT re.scriptHash AS scriptHash, txOut.address AS address, re.purpose as purpose,"
      + " rd.bytes as redeemerBytes, re.unitMem as redeemerMem, re.unitSteps as redeemerSteps,"
      + " d.hash as datumHashIn, d.bytes as datumBytesIn, s.bytes as scriptBytes, txOut.id as txOutId"
      + " FROM Redeemer re"
      + " INNER JOIN Tx tx ON re.tx = tx"
      + " LEFT JOIN RedeemerData rd ON re.redeemerData = rd"
      + " LEFT JOIN TxIn txIn ON (txIn.redeemer = re and txIn.txInput = tx)"
      + " LEFT JOIN TxOut txOut ON txIn.txOut = txOut.tx AND txIn.txOutIndex = txOut.index"
      + " LEFT JOIN Datum d ON (txOut.dataHash = d.hash OR txOut.inlineDatum = d)"
      + " LEFT JOIN Script s ON re.scriptHash = s.hash"
      + " WHERE tx = :tx"
      + " ORDER BY re.id")
  List<TxContractProjection> findContractByTx(@Param("tx") Tx tx);
}
