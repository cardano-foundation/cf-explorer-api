package com.cardano.explorer.repository;

import com.cardano.explorer.entity.Delegation;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface DelegationRepository extends JpaRepository<Delegation, Long> {

  @Query("SELECT count(de) FROM Delegation de WHERE de.activeEpochNo = :activeEpochNo")
  Optional<Integer> numberDelegators(@Param("activeEpochNo") Long activeEpochNo);

  @Query("SELECT count(de) FROM Delegation de WHERE de.poolHash.id = :poolId")
  Optional<Integer> numberDelegatorsByPool(@Param("poolId") Long poolId);
}
