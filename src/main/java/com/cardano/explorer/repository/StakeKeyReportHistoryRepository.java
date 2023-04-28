package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.StakeKeyReportHistory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface StakeKeyReportHistoryRepository extends
    JpaRepository<StakeKeyReportHistory, Long> {

  Page<StakeKeyReportHistory> findByStakeKey(String stakeKey, Pageable pageable);
}
