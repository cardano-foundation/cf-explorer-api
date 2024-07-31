package org.cardanofoundation.explorer.api.repository.explorer;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import org.cardanofoundation.explorer.common.entity.explorer.BlockStatisticsPerEpoch;

public interface BlockStatisticsPerEpochRepository
    extends JpaRepository<BlockStatisticsPerEpoch, Long> {
  Page<BlockStatisticsPerEpoch> findAllByOrderByEpochNoDesc(Pageable pageable);
}
