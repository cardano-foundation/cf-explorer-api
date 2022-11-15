package com.cardano.explorer.repository;

import com.cardano.explorer.entity.TxOut;
import com.cardano.explorer.entity.projection.AddressInputOutput;
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
      + "   INNER JOIN TxIn txIn ON txOut.txId = txIn.txOut.id"
      + "   INNER JOIN Tx tx ON tx.id = txIn.txIn.id AND txIn.txOutIndex = txOut.index"
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
  List<AddressInputOutput> getTxAddressOutputInfo(byte[] hash);


  @Query("SELECT txOut.address AS address,"
      + "   COALESCE(stake.view,txOut.address) AS stakeAddress,"
      + "   txOut.value AS value"
      + " FROM TxOut txOut "
      + " INNER JOIN TxIn txIn ON txOut.tx = txIn.txOut AND txIn.txOutIndex = txOut.index "
      + " INNER JOIN Tx tx ON tx = txIn.txIn"
      + " LEFT JOIN StakeAddress stake "
      + " ON txOut.stakeAddress = stake"
      + " WHERE tx.hash = :hash")
  List<AddressInputOutput> getTxAddressInputInfo(byte[] hash);
}
