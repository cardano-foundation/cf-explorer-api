package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.PoolHash;
import com.sotatek.cardano.common.entity.PoolOfflineData;
import com.sotatek.cardano.common.entity.PoolOfflineData_;
import java.util.List;
import com.sotatek.cardano.common.entity.StakeAddress;
import java.util.Optional;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolOfflineDataRepository extends JpaRepository<PoolOfflineData, Long> {

  Optional<PoolOfflineData> findFirstByPoolOrderByIdDesc(PoolHash pool);

  @EntityGraph(attributePaths = {PoolOfflineData_.POOL})
  @Query("SELECT pool FROM PoolOfflineData pool"
      + " INNER JOIN Delegation delegation ON delegation.poolHash.id = pool.pool.id"
      + " WHERE delegation.id = (SELECT max(id) FROM Delegation where address = :address )")
  Optional<PoolOfflineData> findPoolDataByAddress(StakeAddress address);

  @Query(value = "SELECT po.json FROM PoolOfflineData po WHERE po.pool.id = :poolId ORDER BY po.id DESC")
  List<String> findAllByPool(@Param("poolId") Long poolId);

  @Query(value = "SELECT po FROM PoolOfflineData po WHERE po.pool.id IN :poolIds ORDER BY po.id DESC")
  List<PoolOfflineData> findAllByListPool(@Param("poolIds") List<Long> poolIds);
}
