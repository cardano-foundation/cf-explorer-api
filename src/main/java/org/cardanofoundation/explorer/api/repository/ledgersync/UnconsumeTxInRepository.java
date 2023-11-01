package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.cardanofoundation.explorer.api.projection.AddressInputOutputProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.cardanofoundation.explorer.consumercommon.entity.UnconsumeTxIn;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface UnconsumeTxInRepository extends JpaRepository<UnconsumeTxIn, Long> {
  @Query("SELECT txOut.address AS address, txOut.index AS index, cti.txOut.hash AS txHash,"
      + "   txOut.value AS value, maTxOut.quantity as assetQuantity,"
      + "   asset.name as assetName, asset.fingerprint as assetId"
      + " FROM UnconsumeTxIn cti "
      + " INNER JOIN TxOut txOut on txOut.tx = cti.txOut and txOut.index = cti.txOutIndex "
      + " LEFT JOIN MaTxOut maTxOut ON maTxOut.txOut = txOut"
      + " LEFT JOIN MultiAsset asset ON maTxOut.ident = asset"
      + " WHERE cti.txIn = :tx")
  List<AddressInputOutputProjection> findTxCollateralInput(@Param("tx") Tx tx);
}