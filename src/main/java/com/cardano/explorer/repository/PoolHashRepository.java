package com.cardano.explorer.repository;

import com.cardano.explorer.repository.custom.CustomPoolHashRepository;
import com.sotatek.cardano.common.entity.PoolHash;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface PoolHashRepository extends JpaRepository<PoolHash, Long>,
    CustomPoolHashRepository {

  @Query(value = "SELECT count(ph) FROM PoolHash ph")
  Optional<Integer> countPoolHash();
}
