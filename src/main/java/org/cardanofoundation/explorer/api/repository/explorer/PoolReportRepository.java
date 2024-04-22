package org.cardanofoundation.explorer.api.repository.explorer;

import java.sql.Timestamp;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.common.entity.explorer.PoolReportHistory;

public interface PoolReportRepository extends JpaRepository<PoolReportHistory, Long> {

  @Query(
      "SELECT prh FROM PoolReportHistory prh"
          + " LEFT JOIN ReportHistory rh ON prh.reportHistory.id = rh.id"
          + " WHERE (rh.username = :username)"
          + " AND (rh.createdAt >= :fromDate)"
          + " AND (rh.createdAt <= :toDate)"
          + " AND (:reportName IS NULL OR rh.reportName LIKE :reportName)")
  Page<PoolReportHistory> getPoolReportHistoryByFilter(
      @Param("reportName") String reportName,
      @Param("fromDate") Timestamp fromDate,
      @Param("toDate") Timestamp toDate,
      @Param("username") String username,
      Pageable pageable);
}
