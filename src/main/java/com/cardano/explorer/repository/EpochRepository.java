package com.cardano.explorer.repository;

import com.cardano.explorer.entity.Epoch;
import com.cardano.explorer.repository.custom.CustomEpochRepository;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface EpochRepository extends JpaRepository<Epoch, Long>, CustomEpochRepository {

  Optional<Epoch> findByNo(Integer no);

  @Query(value = "SELECT max(no) FROM Epoch")
  Optional<Integer> findCurrentEpochNo();
}