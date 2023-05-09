package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.consumercommon.entity.PoolReport;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface PoolReportRepository extends JpaRepository<PoolReport, Long> {

  Page<PoolReport> findByUsername(String username, Pageable pageable);

  PoolReport findByUsernameAndId(String username, Long id);

  PoolReport findByReportHistoryId(Long id);
}
