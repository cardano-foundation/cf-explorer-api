package com.cardano.explorer.repository;

import com.cardano.explorer.projection.AddressInputOutputProjection;
import com.sotatek.cardano.common.entity.Tx;
import com.sotatek.cardano.common.entity.TxOut;
import java.math.BigDecimal;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface TxOutRepository extends JpaRepository<TxOut, Long> {


  @Query("SELECT tx.id AS txId, txOut.address AS address"
      + " FROM TxOut txOut"
      + " INNER JOIN Tx tx ON tx.id = txOut.tx.id"
      + " WHERE tx.id IN :txIds"
      + " ORDER BY txOut.index ASC")
  List<AddressInputOutputProjection> findAddressOutputListByTxId(Collection<Long> txIds);


  @Query("SELECT tx.id AS txId, txOut.address AS address"
      + "   FROM TxOut txOut "
      + "   INNER JOIN TxIn txIn ON txOut.tx.id = txIn.txOut.id"
      + "   INNER JOIN Tx tx ON tx.id = txIn.txInput.id AND txIn.txOutIndex = txOut.index"
      + "   WHERE tx.id IN :txIds"
      + "   ORDER BY txIn.id ASC")
  List<AddressInputOutputProjection> findAddressInputListByTxId(Collection<Long> txIds);

  @Query("SELECT txOut.address AS address, COALESCE(stake.view, txOut.address) AS stakeAddress,"
      + "   txOut.value AS value, maTxOut.quantity as assetQuantity,"
      + "   asset.name as assetName, asset.fingerprint as assetId"
      + " FROM TxOut txOut "
      + " LEFT JOIN StakeAddress stake ON txOut.stakeAddress = stake "
      + " LEFT JOIN MaTxOut maTxOut ON maTxOut.txOut = txOut"
      + " LEFT JOIN MultiAsset asset ON maTxOut.ident = asset"
      + " WHERE txOut.tx = :tx")
  List<AddressInputOutputProjection> getTxAddressOutputInfo(Tx tx);


  @Query("SELECT txOut.address AS address, txIn.txOut.hash AS txHash,"
      + "   COALESCE(stake.view,txOut.address) AS stakeAddress,"
      + "   txOut.value AS value, maTxOut.quantity as assetQuantity,"
      + "   asset.name as assetName, asset.fingerprint as assetId"
      + " FROM TxOut txOut "
      + " INNER JOIN TxIn txIn ON txOut.tx = txIn.txOut AND txIn.txOutIndex = txOut.index "
      + " LEFT JOIN StakeAddress stake ON txOut.stakeAddress = stake"
      + " LEFT JOIN MaTxOut maTxOut ON maTxOut.txOut = txOut"
      + " LEFT JOIN MultiAsset asset ON maTxOut.ident = asset"
      + " WHERE txIn.txInput = :tx")
  List<AddressInputOutputProjection> getTxAddressInputInfo(Tx tx);

  @Query("SELECT COALESCE(SUM(txOut.value), 0) AS value FROM TxOut txOut"
      + " INNER JOIN StakeAddress sa ON sa.id = txOut.stakeAddress.id"
      + " WHERE sa.view = :stakeAddress")
  Optional<BigDecimal> getStakeAddressTotalOutput(String stakeAddress);
  @Query("SELECT COALESCE(SUM(txOut.value), 0) AS value "
      + " FROM TxOut txOut "
      + " INNER JOIN TxIn txIn ON txOut.tx = txIn.txOut AND txIn.txOutIndex = txOut.index "
      + " INNER JOIN StakeAddress sa ON sa.id = txOut.stakeAddress.id"
      + " WHERE sa.view = :stakeAddress")
  Optional<BigDecimal> getStakeAddressTotalInput(String stakeAddress);
}
