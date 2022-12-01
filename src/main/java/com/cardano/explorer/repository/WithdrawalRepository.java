package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.Tx;
import com.sotatek.cardano.common.entity.Withdrawal;
import com.sotatek.cardano.common.entity.Withdrawal_;
import java.math.BigDecimal;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface WithdrawalRepository extends JpaRepository<Withdrawal, Long> {
  @EntityGraph(attributePaths = {Withdrawal_.ADDR})
  List<Withdrawal> findByTx(Tx tx);

  @Query("SELECT SUM(w.amount) FROM Withdrawal w "
      + " INNER JOIN StakeAddress stakeAddress ON w.addr.id = stakeAddress.id"
      + " WHERE stakeAddress.view = :stakeAddress")
  Optional<BigDecimal> getRewardWithdrawnByStakeAddress(String stakeAddress);
}
