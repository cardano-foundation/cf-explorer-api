package com.cardano.explorer.repository;

import com.cardano.explorer.projection.TxContract;
import com.sotatek.cardano.common.entity.Redeemer;
import com.sotatek.cardano.common.entity.Tx;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface RedeemerRepository extends JpaRepository<Redeemer, Long> {

  @Query("SELECT re.scriptHash AS scriptHash, tout.address AS address, re.purpose as purpose"
      + " FROM Redeemer re"
      + " INNER JOIN TxOut tout ON re.tx = tout.tx AND re.index = tout.index "
      + " WHERE re.tx = :tx")
  List<TxContract> findContractByTx(Tx tx);
}
