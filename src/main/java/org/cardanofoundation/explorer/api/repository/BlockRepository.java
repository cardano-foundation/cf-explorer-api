package org.cardanofoundation.explorer.api.repository;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolCountProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
import org.cardanofoundation.explorer.consumercommon.entity.Block_;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface BlockRepository
    extends JpaRepository<Block, Long>, JpaSpecificationExecutor<Block> {

  @EntityGraph(attributePaths = {Block_.SLOT_LEADER})
  Optional<Block> findFirstByBlockNo(@Param("no") Long no);

  @EntityGraph(attributePaths = {Block_.SLOT_LEADER})
  Optional<Block> findFirstByHash(@Param("hash") String hash);

  @Query(
      value = "SELECT b FROM Block b WHERE b.epochNo IS NOT NULL",
      countQuery = "SELECT sum (e.blkCount) FROM Epoch e")
  Page<Block> findAllBlock(Pageable pageable);

  @Query(value = "SELECT b FROM Block b " + "WHERE b.blockNo IS NULL")
  List<Block> findGenesisAndEbbBlockInfo();

  @Query(value = "SELECT sum(e.blkCount) FROM Epoch e")
  Long countAllBlock();

  List<Block> findAllByIdIn(@Param("ids") Collection<Long> ids);

  @Query(value = "SELECT b FROM Block b WHERE b.epochNo = :epochNo")
  Page<Block> findBlockByEpochNo(@Param("epochNo") Integer epochNo, Pageable pageable);

  @Query(value = "SELECT max(blockNo) FROM Block")
  Optional<Integer> findCurrentBlock();

  @Query(value = "SELECT max(epochSlotNo) FROM Block WHERE epochNo = :epochNo")
  Integer findCurrentSlotByEpochNo(@Param("epochNo") Integer epochNo);

  @Query(
      value =
          "SELECT count(bk.id) FROM PoolHash ph "
              + "JOIN SlotLeader sl ON sl.poolHash.id = ph.id "
              + "JOIN Block bk ON bk.slotLeader.id = sl.id "
              + "WHERE ph.id = :poolId")
  Integer getCountBlockByPool(@Param("poolId") Long poolId);

  @Query(
      value =
          "SELECT count(bk.id) FROM PoolHash ph "
              + "JOIN SlotLeader sl ON sl.poolHash.id = ph.id "
              + "JOIN Block bk ON bk.slotLeader.id = sl.id "
              + "WHERE ph.id = :poolId AND bk.epochNo = (SELECT max(ep.no) FROM Epoch ep)")
  Integer getCountBlockByPoolAndCurrentEpoch(@Param("poolId") Long poolId);

  @Query(
      value =
          "SELECT ph.id AS poolId, count(bk.id) AS countValue "
              + "FROM PoolHash ph "
              + "JOIN SlotLeader sl ON sl.poolHash.id = ph.id "
              + "JOIN Block bk ON bk.slotLeader.id = sl.id "
              + "WHERE ph.id IN :poolIds AND bk.epochNo = :epochNo "
              + "GROUP BY ph.id")
  List<PoolCountProjection> getCountBlockByPoolsAndCurrentEpoch(
      @Param("poolIds") Set<Long> poolIds, @Param("epochNo") Integer epochNo);

  @Query(
      value =
          "SELECT ph.id AS poolId, count(bk.id) AS countValue "
              + "FROM PoolHash ph "
              + "JOIN SlotLeader sl ON sl.poolHash.id = ph.id "
              + "JOIN Block bk ON bk.slotLeader.id = sl.id "
              + "WHERE ph.id IN :poolIds "
              + "GROUP BY ph.id")
  List<PoolCountProjection> getCountBlockByPools(@Param("poolIds") Set<Long> poolIds);

  @Query(
      value =
          "SELECT ph.id AS poolId, ph.view AS poolView, count(bk.id) AS countValue "
              + "FROM PoolHash ph "
              + "JOIN SlotLeader sl ON sl.poolHash.id = ph.id "
              + "JOIN Block bk ON bk.slotLeader.id = sl.id "
              + "WHERE bk.epochNo = :epochNo "
              + "GROUP BY ph.id, ph.view "
              + "ORDER BY countValue DESC")
  List<PoolCountProjection> findTopDelegationByEpochBlock(
      @Param("epochNo") Integer epochNo, Pageable pageable);
}
