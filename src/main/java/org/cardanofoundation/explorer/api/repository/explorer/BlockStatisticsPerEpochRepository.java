package org.cardanofoundation.explorer.api.repository.explorer;

import java.util.List;

import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import org.cardanofoundation.explorer.common.entity.explorer.BlockStatisticsPerEpoch;

public interface BlockStatisticsPerEpochRepository
    extends JpaRepository<BlockStatisticsPerEpoch, Long> {
  List<BlockStatisticsPerEpoch> findAllByOrderByEpochNoDesc(Pageable pageable);
}
