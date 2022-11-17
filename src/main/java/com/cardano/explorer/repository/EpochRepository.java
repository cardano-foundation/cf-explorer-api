package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.Epoch;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface EpochRepository extends JpaRepository<Epoch, Long> {

  Optional<Epoch> findByNo(Integer no);

  @Query(value = "SELECT max(no) FROM Epoch")
  Optional<Integer> findCurrentEpochNo();
}