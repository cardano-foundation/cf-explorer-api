package org.cardanofoundation.explorer.api.repository;

import java.sql.Timestamp;
import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.consumercommon.entity.EpochParam;

@Repository
public interface EpochParamRepository extends JpaRepository<EpochParam, Long> {

  Optional<EpochParam> findEpochParamByEpochNo(@Param("epochNo") Integer epochNo);
  @Query(value = "SELECT ep "
      + "FROM EpochParam ep "
      + "JOIN Epoch e ON e.no = ep.epochNo "
      + "WHERE e.startTime <=  :epochTime")
  List<EpochParam> findEpochParamInTime(@Param("epochTime") Timestamp epochTime);
}
