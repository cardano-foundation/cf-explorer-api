package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.StakeKeyReportHistory;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface StakeKeyReportHistoryRepository extends
    JpaRepository<StakeKeyReportHistory, Long> {

  Page<StakeKeyReportHistory> findByStakeKey(String stakeKey, Pageable pageable);

  @Query("select srh from StakeKeyReportHistory srh"
      + " left join ReportHistory rh on srh.reportHistory.id = rh.id"
      + " WHERE rh.storageKey is null or rh.storageKey = ''")
  List<StakeKeyReportHistory> findByStorageKeyNull();
}
