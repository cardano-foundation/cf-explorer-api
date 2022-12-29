package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.PoolHash;
import com.sotatek.cardano.common.entity.PoolOfflineData;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolOfflineDataRepository extends JpaRepository<PoolOfflineData, Long> {

  Optional<PoolOfflineData> findFirstByPoolOrderByIdDesc(PoolHash pool);

  @Query(value = "SELECT po FROM PoolOfflineData po WHERE po.pool.id IN :poolIds ORDER BY po.id DESC")
  List<PoolOfflineData> findAllByListPool(@Param("poolIds") List<Long> poolIds);
}
