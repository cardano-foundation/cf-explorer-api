package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.Block;
import com.sotatek.cardano.common.entity.Block_;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface BlockRepository extends JpaRepository<Block, Long>,
    JpaSpecificationExecutor<Block> {

  @EntityGraph(attributePaths = {Block_.SLOT_LEADER, Block_.TX_LIST})
  Optional<Block> findFirstByBlockNo(Long no);

  @EntityGraph(attributePaths = {Block_.SLOT_LEADER, Block_.TX_LIST})
  Optional<Block> findFirstByHash(String hash);

  Page<Block> findAll(Pageable pageable);

  Page<Block> findAll(Specification specification, Pageable pageable);

  @Query(value = "SELECT max(blockNo) FROM Block")
  Optional<Integer> findCurrentBlock();

  @Query(value = "SELECT max(epochSlotNo) FROM Block WHERE epochNo = :epochNo")
  Integer findCurrentSlotByEpochNo(@Param("epochNo") Integer epochNo);

  @Query(value = "SELECT count(bk.id) FROM PoolHash ph "
      + "JOIN SlotLeader sl ON sl.poolHash.id = ph.id "
      + "JOIN Block bk ON bk.slotLeader.id = sl.id "
      + "WHERE ph.id = :poolId")
  Integer getCountBlockByPool(@Param("poolId") Long poolId);

  @Query(value = "SELECT count(bk.id) FROM PoolHash ph "
      + "JOIN SlotLeader sl ON sl.poolHash.id = ph.id "
      + "JOIN Block bk ON bk.slotLeader.id = sl.id "
      + "WHERE ph.id = :poolId AND bk.epochNo = (SELECT max(ep.no) FROM Epoch ep)")
  Integer getCountBlockByPoolAndCurrentEpoch(@Param("poolId") Long poolId);
}
