package com.cardano.explorer.repository;

import com.cardano.explorer.entity.PoolHash;
import com.cardano.explorer.entity.PoolOfflineData;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolOfflineDataRepository extends JpaRepository<PoolOfflineData, Long> {

  Optional<PoolOfflineData> findFirstByPool(PoolHash pool);
}
