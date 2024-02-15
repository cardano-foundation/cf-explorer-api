package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.AddressInputOutputProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.FailedTxOut;
import org.cardanofoundation.explorer.common.entity.ledgersync.Tx;

public interface FailedTxOutRepository extends JpaRepository<FailedTxOut, Long> {
  @Query(
      "SELECT failedTxOut.address AS address, tx.hash AS txHash, failedTxOut.value AS value,"
          + " failedTxOut.index AS index, failedTxOut.multiAssetsDescr AS assetsJson"
          + " FROM FailedTxOut failedTxOut "
          + " INNER JOIN Tx tx ON failedTxOut.tx = tx "
          + " WHERE tx = :tx")
  List<AddressInputOutputProjection> findFailedTxOutByTx(@Param("tx") Tx tx);
}
