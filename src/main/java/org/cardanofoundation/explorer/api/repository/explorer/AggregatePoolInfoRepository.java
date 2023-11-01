package org.cardanofoundation.explorer.api.repository.explorer;

import java.util.Collection;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolListProjection;
import org.cardanofoundation.explorer.consumercommon.explorer.entity.AggregatePoolInfo;

public interface AggregatePoolInfoRepository extends JpaRepository<AggregatePoolInfo, Long> {

  List<AggregatePoolInfo> getAllByPoolIdIn(Collection<Long> poolIds);

  AggregatePoolInfo findByPoolId(Long poolId);

  @Query(value = "SELECT ap.poolId as poolId, COALESCE(ap.delegatorCount, 0) as numberDelegators, "
      + "COALESCE(ap.blockInEpoch, 0) as epochBlock, "
      + "COALESCE(ap.blockLifeTime, 0) as lifetimeBlock "
      + "from AggregatePoolInfo ap "
      + "WHERE ap.poolId NOT IN :exceptPoolIds")
  Page<PoolListProjection> findAllByPoolIdNotIn(@Param("exceptPoolIds") Collection<Long> exceptPoolIds,
                                             Pageable pageable);
}
