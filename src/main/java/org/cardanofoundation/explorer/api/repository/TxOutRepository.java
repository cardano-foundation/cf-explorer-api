package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.projection.AddressInputOutputProjection;
import org.cardanofoundation.explorer.api.projection.TxContractProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.cardanofoundation.explorer.consumercommon.entity.TxOut;
import java.util.Collection;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface TxOutRepository extends JpaRepository<TxOut, Long> {


  @Query("SELECT tx.id AS txId, txOut.address AS address"
      + " FROM TxOut txOut"
      + " INNER JOIN Tx tx ON tx.id = txOut.tx.id"
      + " WHERE tx.id IN :txIds"
      + " ORDER BY txOut.index ASC")
  List<AddressInputOutputProjection> findAddressOutputListByTxId(@Param("txIds") Collection<Long> txIds);


  @Query("SELECT tx.id AS txId, txOut.address AS address"
      + "   FROM TxOut txOut "
      + "   INNER JOIN TxIn txIn ON txOut.tx.id = txIn.txOut.id"
      + "   INNER JOIN Tx tx ON tx.id = txIn.txInput.id AND txIn.txOutIndex = txOut.index"
      + "   WHERE tx.id IN :txIds"
      + "   ORDER BY txIn.id ASC")
  List<AddressInputOutputProjection> findAddressInputListByTxId(@Param("txIds") Collection<Long> txIds);

  @Query("SELECT txOut.address AS address, txOut.index as index, COALESCE(stake.view, txOut.address) AS stakeAddress,"
      + "   txOut.value AS value, maTxOut.quantity as assetQuantity,"
      + "   stake.view as stakeView, "
      + "   asset.name as assetName, asset.fingerprint as assetId, asset.id as multiAssetId"
      + " FROM TxOut txOut "
      + " LEFT JOIN StakeAddress stake ON txOut.stakeAddress = stake "
      + " LEFT JOIN MaTxOut maTxOut ON maTxOut.txOut = txOut"
      + " LEFT JOIN MultiAsset asset ON maTxOut.ident = asset"
      + " WHERE txOut.tx = :tx")
  List<AddressInputOutputProjection> getTxAddressOutputInfo(@Param("tx") Tx tx);


  @Query("SELECT txOut.address AS address, txOut.index as index, txIn.txOut.hash AS txHash,"
      + "   COALESCE(stake.view,txOut.address) AS stakeAddress,"
      + "   stake.view as stakeView, "
      + "   txOut.value AS value, maTxOut.quantity as assetQuantity,"
      + "   asset.name as assetName, asset.fingerprint as assetId, asset.id as multiAssetId"
      + " FROM TxOut txOut "
      + " INNER JOIN TxIn txIn ON txOut.tx = txIn.txOut AND txIn.txOutIndex = txOut.index "
      + " LEFT JOIN StakeAddress stake ON txOut.stakeAddress = stake"
      + " LEFT JOIN MaTxOut maTxOut ON maTxOut.txOut = txOut"
      + " LEFT JOIN MultiAsset asset ON maTxOut.ident = asset"
      + " WHERE txIn.txInput = :tx")
  List<AddressInputOutputProjection> getTxAddressInputInfo(@Param("tx") Tx tx);

  @Query("SELECT txOut.id as txOutId, txOut.address as address, d.hash as datumHashOut, d.bytes as datumBytesOut"
      + " FROM TxOut txOut"
      + " JOIN Datum d ON txOut.inlineDatum = d"
      + " WHERE txOut.tx = :tx")
  List<TxContractProjection> getContractDatumOutByTx(@Param("tx") Tx tx);
}
