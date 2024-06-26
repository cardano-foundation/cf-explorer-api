package org.cardanofoundation.explorer.api.repository.explorer;

import java.sql.Timestamp;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.ReportHistoryProjection;
import org.cardanofoundation.explorer.common.entity.explorer.ReportHistory;

public interface ReportHistoryRepository extends JpaRepository<ReportHistory, Long> {

  @Query(
      "SELECT rh.createdAt AS createdAt,"
          + " rh.reportName AS reportName, rh.status AS status, rh.type AS type,"
          + " skrh.id AS stakeKeyReportId, prh.id AS poolReportId"
          + " FROM ReportHistory rh "
          + " LEFT JOIN StakeKeyReportHistory skrh ON skrh.reportHistory.id = rh.id"
          + " LEFT JOIN PoolReportHistory prh ON prh.reportHistory.id = rh.id "
          + " WHERE 1 = 1"
          + " AND (rh.username = :username)"
          + " AND (rh.createdAt >= :fromDate)"
          + " AND (rh.createdAt <= :toDate)"
          + " AND (rh.type = 'STAKE_KEY' OR rh.type = 'POOL_ID')"
          + " AND (:reportName IS NULL OR rh.reportName LIKE :reportName)")
  Page<ReportHistoryProjection> getRecordHistoryByFilter(
      @Param("reportName") String reportName,
      @Param("fromDate") Timestamp fromDate,
      @Param("toDate") Timestamp toDate,
      @Param("username") String username,
      Pageable pageable);

  @Query(
      "SELECT COUNT(rh.id) FROM ReportHistory rh "
          + " WHERE 1 = 1"
          + " AND (rh.username = :username)"
          + " AND (rh.createdAt >= :fromDate)"
          + " AND (rh.createdAt <= :toDate)")
  Integer countByUsernameAndCreatedAtBetween(
      @Param("username") String username,
      @Param("fromDate") Timestamp fromDate,
      @Param("toDate") Timestamp toDate);
}
