package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.projection.ReferenceInputProjection;
import org.cardanofoundation.explorer.consumercommon.entity.ReferenceTxIn;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface ReferenceTxInRepository extends JpaRepository<ReferenceTxIn, Long> {
  @Query("SELECT txOut.address as address, tx.hash as txHash, txOut.value as value, r.txOutIndex as index, txOut.id as txOutId,"
      + " script.hash as scriptHash, script.bytes as scriptBytes, datum.hash as datumHash, datum.bytes as datumBytes"
      + " FROM ReferenceTxIn r"
      + " INNER JOIN TxOut txOut ON r.txOut = txOut.tx AND txOut.index = r.txOutIndex"
      + " INNER JOIN Tx tx ON txOut.tx = tx"
      + " LEFT JOIN Script script ON txOut.referenceScript = script"
      + " LEFT JOIN Datum datum ON txOut.inlineDatum = datum"
      + " LEFT JOIN MaTxOut maTxOut ON maTxOut.txOut = txOut"
      + " LEFT JOIN MultiAsset asset ON maTxOut.ident = asset"
      + " WHERE r.txIn = :tx")
  List<ReferenceInputProjection> getReferenceInputByTx(@Param("tx") Tx tx);

}
