package com.cardano.explorer.repository;

import com.cardano.explorer.projection.CollateralInputOutputProjection;
import com.sotatek.cardano.common.entity.Tx;
import com.sotatek.cardano.common.entity.UnconsumeTxIn;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface CollateralTxInRepository extends JpaRepository<UnconsumeTxIn, Long> {
  @Query("SELECT tout.address AS address, txOut.hash AS txHash, tout.value AS value "
      + " FROM UnconsumeTxIn cti "
      + " INNER JOIN Tx txIn ON cti.txIn = txIn "
      + " INNER JOIN Tx txOut ON cti.txOut = txOut "
      + " INNER JOIN TxOut tout on tout.tx = cti.txOut and tout.index = cti.txOutIndex "
      + " WHERE txIn = :tx")
  List<CollateralInputOutputProjection> findTxCollateralInput(Tx tx);
}