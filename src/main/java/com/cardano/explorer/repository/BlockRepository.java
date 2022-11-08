package com.cardano.explorer.repository;

import com.cardano.explorer.entity.Block;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface BlockRepository extends JpaRepository<Block, Long> {

  Optional<Block> findByBlockNo(Integer no);

  @Query(value = "SELECT block FROM Block block")
  List<Block> findAllBlock(Pageable pageable);

  Page<Block> findAll(Pageable pageable);

  @Query(value = "SELECT count(b) FROM Block b")
  Optional<Integer> countAll();

  Page<Block> findByEpochNo(Integer epochNo, Pageable pageable);

  List<Block> findBlockByIdIn(List<Long> blockIds);

  @Query(value = "SELECT max(blockNo) FROM Block")
  Optional<Integer> findCurrentBlock();
}
