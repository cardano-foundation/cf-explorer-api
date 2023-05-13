package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.consumercommon.entity.PoolReportHistory;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface PoolReportRepository extends JpaRepository<PoolReportHistory, Long> {

  @Query("SELECT prh FROM PoolReportHistory prh"
      + " LEFT JOIN ReportHistory rh ON prh.reportHistory.id = rh.id"
      + " WHERE rh.username = :username")
  Page<PoolReportHistory> findByUsername(@Param("username") String username, Pageable pageable);

  @Query("SELECT prh FROM PoolReportHistory prh"
      + " LEFT JOIN ReportHistory rh ON prh.reportHistory.id = rh.id"
      + " WHERE rh.username = :username"
      + " AND prh.id = :id")
  PoolReportHistory findByUsernameAndId(@Param("username") String username, @Param("id") Long id);

  PoolReportHistory findByReportHistoryId(@Param("reportHistoryId") Long reportHistoryId);
}
