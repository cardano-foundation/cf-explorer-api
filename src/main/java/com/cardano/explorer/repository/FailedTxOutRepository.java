package com.cardano.explorer.repository;

import com.cardano.explorer.projection.AddressInputOutputProjection;
import com.sotatek.cardano.common.entity.FailedTxOut;
import com.sotatek.cardano.common.entity.Tx;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface FailedTxOutRepository extends JpaRepository<FailedTxOut, Long> {
  @Query("SELECT failedTxOut.address AS address, tx.hash AS txHash, failedTxOut.value AS value,"
      + " failedTxOut.index AS index, failedTxOut.multiAssetsDescr AS assetsJson"
      + " FROM FailedTxOut failedTxOut "
      + " INNER JOIN Tx tx ON failedTxOut.tx = tx "
      + " WHERE tx = :tx")
  List<AddressInputOutputProjection> findFailedTxOutByTx(Tx tx);
}