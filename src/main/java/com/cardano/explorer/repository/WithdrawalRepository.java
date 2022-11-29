package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.Tx;
import com.sotatek.cardano.common.entity.Withdrawal;
import com.sotatek.cardano.common.entity.Withdrawal_;
import java.math.BigDecimal;
import java.util.List;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;

public interface WithdrawalRepository extends JpaRepository<Withdrawal, Long> {
  @EntityGraph(attributePaths = {Withdrawal_.ADDR})
  List<Withdrawal> findByTx(Tx tx);
}
