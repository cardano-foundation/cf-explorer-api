package org.cardanofoundation.explorer.api.repository.explorer;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import org.cardanofoundation.explorer.common.entity.explorer.BlockStatisticsDaily;

public interface BlockStatisticsDailyRepository extends JpaRepository<BlockStatisticsDaily, Long> {
  Page<BlockStatisticsDaily> findAllByOrderByTimeDesc(Pageable pageable);
}
