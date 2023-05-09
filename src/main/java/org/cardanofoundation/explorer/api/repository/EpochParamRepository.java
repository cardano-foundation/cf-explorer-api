package org.cardanofoundation.explorer.api.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.consumercommon.entity.EpochParam;

@Repository
public interface EpochParamRepository extends JpaRepository<EpochParam, Long> {

  @Query("SELECT  ep "
      + "FROM EpochParam ep "
      + "WHERE ep.id = (SELECT MAX(ep1.id) FROM EpochParam ep1)")
  EpochParam findTopEpochParam();
}
