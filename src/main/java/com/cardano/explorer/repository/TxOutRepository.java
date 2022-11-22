package com.cardano.explorer.repository;

import com.cardano.explorer.projection.AddressInputOutput;
import com.sotatek.cardano.common.entity.TxOut;
import java.util.Collection;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface TxOutRepository extends JpaRepository<TxOut, Long> {


  @Query("SELECT tx.id AS txId, txOut.address AS address"
      + " FROM TxOut txOut"
      + " INNER JOIN Tx tx ON tx.id = txOut.tx.id"
      + " WHERE tx.id IN :txIds"
      + " ORDER BY txOut.index ASC")
  List<AddressInputOutput> findAddressOutputListByTxId(Collection<Long> txIds);


  @Query("SELECT tx.id AS txId, txOut.address AS address"
      + "   FROM TxOut txOut "
      + "   INNER JOIN TxIn txIn ON txOut.tx.id = txIn.txOut.id"
      + "   INNER JOIN Tx tx ON tx.id = txIn.txInput.id AND txIn.txOutIndex = txOut.index"
      + "   WHERE tx.id IN :txIds"
      + "   ORDER BY txIn.id ASC")
  List<AddressInputOutput> findAddressInputListByTxId(Collection<Long> txIds);

  @Query("SELECT txOut.address AS address, COALESCE(stake.view, txOut.address) AS stakeAddress,"
      + "   txOut.value AS value"
      + " FROM TxOut txOut "
      + " LEFT JOIN StakeAddress stake "
      + " ON txOut.stakeAddress = stake "
      + " INNER JOIN Tx tx ON txOut.tx = tx"
      + " WHERE tx.hash = :hash")
  List<AddressInputOutput> getTxAddressOutputInfo(String hash);


  @Query("SELECT txOut.address AS address,"
      + "   COALESCE(stake.view,txOut.address) AS stakeAddress,"
      + "   txOut.value AS value"
      + " FROM TxOut txOut "
      + " INNER JOIN TxIn txIn ON txOut.tx = txIn.txOut AND txIn.txOutIndex = txOut.index "
      + " INNER JOIN Tx tx ON tx = txIn.txInput"
      + " LEFT JOIN StakeAddress stake "
      + " ON txOut.stakeAddress = stake"
      + " WHERE tx.hash = :hash")
  List<AddressInputOutput> getTxAddressInputInfo(String hash);
}
