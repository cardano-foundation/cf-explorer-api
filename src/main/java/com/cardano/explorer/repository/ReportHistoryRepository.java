package com.cardano.explorer.repository;

import com.cardano.explorer.projection.ReportHistoryProjection;
import com.sotatek.cardano.common.entity.ReportHistory;

import java.sql.Timestamp;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface ReportHistoryRepository extends JpaRepository<ReportHistory, Long> {

  @Query("select rh.createdAt as createdAt,"
      + " rh.reportName as reportName, rh.status as status, rh.type as type,"
      + " skrh.id as stakeKeyReportId, prh.id as poolReportId"
      + " from ReportHistory rh "
      + " left join StakeKeyReportHistory skrh on skrh.reportHistory.id = rh.id"
      + " left join PoolReport prh on prh.reportHistory.id = rh.id "
      + " where 1 = 1"
      + " and (rh.username = :username)"
      + " and (rh.createdAt >= :fromDate)"
      + " and (rh.createdAt <= :toDate)"
      + " and :reportName is null or rh.reportName like :reportName")
  Page<ReportHistoryProjection> getRecordHistoryByFilter(@Param("reportName") String reportName,
                                                         @Param("fromDate") Timestamp fromDate,
                                                         @Param("toDate") Timestamp toDate,
                                                         @Param("username") String username,
                                                         Pageable pageable);
}
