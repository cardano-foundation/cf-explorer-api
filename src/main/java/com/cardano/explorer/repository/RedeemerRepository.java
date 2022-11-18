package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.Redeemer;
import com.sotatek.cardano.common.entity.Tx;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface RedeemerRepository extends JpaRepository<Redeemer, Long> {
  List<Redeemer> findByTx(Tx tx);
}
