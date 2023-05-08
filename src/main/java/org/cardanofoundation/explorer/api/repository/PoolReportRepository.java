package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.consumercommon.entity.PoolReport;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface PoolReportRepository extends JpaRepository<PoolReport, Long> {

  @Query("select pr from PoolReport pr"
      + " left join ReportHistory rh on pr.reportHistory.id = rh.id"
      + " WHERE rh.storageKey is null or rh.storageKey = ''")
  List<PoolReport> findByStorageKeyNull();

  Page<PoolReport> findByUsername(String username, Pageable pageable);

  PoolReport findByUsernameAndId(String username, Long id);
}
