package com.cardano.explorer.repository;

import com.cardano.explorer.entity.Tx;
import com.cardano.explorer.entity.TxOut;
import com.cardano.explorer.entity.projection.AddressInputOutput;
import java.util.List;
import java.util.Set;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface TxOutRepository extends JpaRepository<TxOut, Long> {


  List<AddressInputOutput> findByTxInOrderByIndexAsc(List<Tx> tx);


  @Query("SELECT tx.id as txId, txOut.address as address"
      + "   FROM TxOut txOut "
      + "   INNER JOIN TxIn txIn On txOut.txId = txIn.txOut.id"
      + "   INNER JOIN Tx tx on tx.id = txIn.txIn.id and txIn.txOutIndex = txOut.index"
      + "   WHERE tx.id IN :txIds"
      + "   ORDER BY txIn.id asc")
  List<AddressInputOutput> findAddressInputByTxIdIn(Set<Long> txIds);
}
