package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.consumercommon.entity.StakeKeyReportHistory;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface StakeKeyReportHistoryRepository extends
    JpaRepository<StakeKeyReportHistory, Long> {

  @Query("SELECT srh FROM StakeKeyReportHistory srh"
      + " LEFT JOIN ReportHistory rh ON srh.reportHistory.id = rh.id"
      + " WHERE rh.username = :username"
      + " AND srh.stakeKey = :stakeKey")
  Page<StakeKeyReportHistory> findByUsernameAndStakeKey(@Param("stakeKey") String stakeKey,
                                                        @Param("username") String username,
                                                        Pageable pageable);

  @Query("SELECT srh FROM StakeKeyReportHistory srh"
      + " LEFT JOIN ReportHistory rh ON srh.reportHistory.id = rh.id"
      + " WHERE rh.username = :username")
  Page<StakeKeyReportHistory> findByUsername(@Param("username") String username,
                                             Pageable pageable);

  StakeKeyReportHistory findByReportHistoryId(@Param("reportHistoryId") Long reportHistoryId);
}
