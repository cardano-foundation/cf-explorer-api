package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.Delegation;
import com.sotatek.cardano.common.entity.Tx;
import java.util.List;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface DelegationRepository extends JpaRepository<Delegation, Long> {

  @Query("SELECT count(de.id) FROM Delegation de")
  Integer numberDelegatorsAllPool();

  @Query("SELECT count(de.id) FROM Delegation de WHERE de.poolHash.id = :poolId")
  Integer numberDelegatorsByPool(@Param("poolId") Long poolId);

  @EntityGraph(attributePaths = {"poolHash", "address"})
  List<Delegation> findByTx(Tx tx);
}
