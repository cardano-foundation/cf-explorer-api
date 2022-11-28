package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.Tx;
import com.sotatek.cardano.common.entity.Withdrawal;
import java.util.List;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;

public interface WithdrawalRepository extends JpaRepository<Withdrawal, Long> {
  @EntityGraph(attributePaths = {"addr"})
  List<Withdrawal> findByTx(Tx tx);
}
