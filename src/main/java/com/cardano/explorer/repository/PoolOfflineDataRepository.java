package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.PoolHash;
import com.sotatek.cardano.common.entity.PoolOfflineData;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolOfflineDataRepository extends JpaRepository<PoolOfflineData, Long> {

  Optional<PoolOfflineData> findFirstByPool(PoolHash pool);
}
