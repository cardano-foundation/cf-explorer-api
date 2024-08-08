package org.cardanofoundation.explorer.api.repository.explorer;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import org.cardanofoundation.explorer.api.projection.BlockPropagationProjection;
import org.cardanofoundation.explorer.common.entity.explorer.BlockStatisticsDaily;

public interface BlockStatisticsDailyRepository extends JpaRepository<BlockStatisticsDaily, Long> {

  @Query(
      """
    SELECT bsd.time AS time, bsd.epochNo AS epochNo,
     bsd.blockPropMean AS blockPropMean, bsd.blockPropMedian AS blockPropMedian,
     bsd.blockPropP90 AS blockPropP90, bsd.blockPropP95 AS blockPropP95
    FROM BlockStatisticsDaily bsd
    """)
  Page<BlockPropagationProjection> findBlockPropagation(Pageable pageable);
}
