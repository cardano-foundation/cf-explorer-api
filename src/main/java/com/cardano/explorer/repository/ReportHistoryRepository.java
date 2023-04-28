package com.cardano.explorer.repository;

import com.cardano.explorer.projection.ReportHistoryProjection;
import com.sotatek.cardano.common.entity.ReportHistory;
import java.sql.Timestamp;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface ReportHistoryRepository extends JpaRepository<ReportHistory, Long> {

  @Query("select skrh.id as id, rh.createdAt as createdAt,"
      + " rh.reportName as reportName, rh.status as status, rh.type as type"
      + " from ReportHistory rh left join StakeKeyReportHistory skrh on skrh.reportHistory.id = rh.id"
      + " where 1 = 1"
      + " and (rh.createdAt >= :fromDate)"
      + " and (rh.createdAt <= :toDate)"
      + " and :reportName is null or rh.reportName like :reportName")
  Page<ReportHistoryProjection> getRecordHistoryByFilter(String reportName, Timestamp fromDate,
      Timestamp toDate, Pageable pageable);
}
