package org.cardanofoundation.explorer.api.repository.explorer;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import org.cardanofoundation.explorer.api.projection.BlockPropagationProjection;
import org.cardanofoundation.explorer.common.entity.explorer.BlockStatisticsPerEpoch;

public interface BlockStatisticsPerEpochRepository
    extends JpaRepository<BlockStatisticsPerEpoch, Long> {
  @Query(
      """
    SELECT bspe.time AS time, bspe.epochNo AS epochNo,
     bspe.blockPropMean AS blockPropMean, bspe.blockPropMedian AS blockPropMedian,
     bspe.blockPropP90 AS blockPropP90, bspe.blockPropP95 AS blockPropP95
    FROM BlockStatisticsPerEpoch bspe
    """)
  Page<BlockPropagationProjection> findBlockPropagation(Pageable pageable);
}
