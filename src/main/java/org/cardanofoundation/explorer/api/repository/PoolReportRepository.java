package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.consumercommon.entity.PoolReport;
import org.cardanofoundation.explorer.consumercommon.entity.StakeKeyReportHistory;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface PoolReportRepository extends JpaRepository<PoolReport, Long> {

  @Query("select pr from PoolReport pr"
      + " left join ReportHistory rh on pr.reportHistory.id = rh.id"
      + " WHERE rh.storageKey is null or rh.storageKey = ''")
  List<PoolReport> findByStorageKeyNull();
}
