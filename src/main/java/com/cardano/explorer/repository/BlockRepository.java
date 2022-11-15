package com.cardano.explorer.repository;

import com.cardano.explorer.entity.Block;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface BlockRepository extends JpaRepository<Block, Long>,
    JpaSpecificationExecutor<Block> {

  @EntityGraph(attributePaths = {"slotLeader", "txList"})
  Optional<Block> findByBlockNo(Integer no);

  @EntityGraph(attributePaths = {"slotLeader"})
  Page<Block> findAll(Pageable pageable);

  @EntityGraph(attributePaths = {"slotLeader"})
  Page<Block> findAll(Specification specification, Pageable pageable);

  @Query(value = "SELECT max(blockNo) FROM Block")
  Optional<Integer> findCurrentBlock();
}
