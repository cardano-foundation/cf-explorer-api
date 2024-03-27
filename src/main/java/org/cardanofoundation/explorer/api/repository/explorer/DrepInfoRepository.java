package org.cardanofoundation.explorer.api.repository.explorer;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.common.entity.explorer.DRepInfo;

public interface DrepInfoRepository extends JpaRepository<DRepInfo, Long> {

  @Query(
      value =
          " select dri from DRepInfo dri"
              + " where dri.drepHash = :dRepHashOrDRepId or dri.drepId = :dRepHashOrDRepId")
  DRepInfo findByDRepHashOrDRepId(@Param("dRepHashOrDRepId") String dRepHashOrDRepId);
}
