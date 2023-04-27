package com.cardano.explorer.repository;

import com.cardano.explorer.projection.ReportHistoryProjection;
import com.sotatek.cardano.common.entity.ReportHistory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface ReportHistoryRepository extends JpaRepository<ReportHistory, Long> {

  @Query("select skrh.id as id, rh.createdAt as createdAt,"
      + " rh.reportName as reportName, rh.status as status, rh.type as type"
      + " from ReportHistory rh left join StakeKeyReportHistory skrh on skrh.reportHistory.id = rh.id"
      + " order by rh.createdAt desc")
  Page<ReportHistoryProjection> getAlLRecordHistory(Pageable pageable);
}
