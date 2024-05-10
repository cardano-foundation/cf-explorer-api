package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.api.model.response.pool.projection.PoolCountProjection;
import org.cardanofoundation.explorer.api.projection.PoolMintBlockProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.Block;
import org.cardanofoundation.explorer.common.entity.ledgersync.Block_;

@Repository
public interface BlockRepository
    extends JpaRepository<Block, Long>, JpaSpecificationExecutor<Block> {

  @EntityGraph(attributePaths = {Block_.SLOT_LEADER})
  Optional<Block> findFirstByBlockNo(@Param("no") Long no);

  @EntityGraph(attributePaths = {Block_.SLOT_LEADER})
  Optional<Block> findFirstByHash(@Param("hash") String hash);

  @Query(value = "SELECT b FROM Block b", countQuery = "SELECT sum (e.blkCount) + 1 FROM Epoch e")
  Page<Block> findAllBlock(Pageable pageable);

  @Query(
      value = "SELECT count(b) FROM Block b " + "WHERE b.blockNo IS NULL " + "AND b.id < :blockId")
  Optional<Long> countGenesisAndEbbBlockInfoByBlockIdLessThan(@Param("blockId") Long blockId);

  @Query(
      value = "SELECT count(b) FROM Block b " + "WHERE b.blockNo IS NULL " + "AND b.id > :blockId")
  Optional<Long> countGenesisAndEbbBlockInfoByBlockIdGreaterThan(@Param("blockId") Long blockId);

  @Query("SELECT min(b.blockNo) FROM Block b")
  Long findMinBlockNo();

  @Query(
      value = "SELECT b.id FROM Block b " + "WHERE b.id > :blockId " + "ORDER BY b.id ASC LIMIT 1")
  Long findNextBlockId(@Param("blockId") Long blockId);

  @Query(value = "SELECT sum(e.blkCount) FROM Epoch e")
  Long countAllBlock();

  List<Block> findAllByIdIn(@Param("ids") Collection<Long> ids);

  @Query(value = "SELECT b FROM Block b WHERE b.epochNo = :epochNo")
  Page<Block> findBlockByEpochNo(@Param("epochNo") Integer epochNo, Pageable pageable);

  @Query(value = "SELECT max(blockNo) FROM Block")
  Optional<Integer> findCurrentBlock();

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

  @Query(value = "SELECT b from Block b WHERE b.id = (SELECT max(b2.id) FROM Block b2)")
  Block findCurrentBlockById();

  @Query("SELECT b FROM Block b WHERE b.blockNo IS NOT NULL ORDER BY b.blockNo DESC LIMIT 1")
  Optional<Block> findLatestBlock();

  @Query("SELECT b FROM Block b WHERE b.blockNo IS NOT NULL ORDER BY b.blockNo ASC LIMIT 1")
  Optional<Block> findFirstBlock();

  @Query(
      "SELECT b FROM Block b"
          + " INNER JOIN Epoch e ON e.no = b.epochNo"
          + " WHERE e.era != org.cardanofoundation.explorer.common.entity.enumeration.EraType.BYRON"
          + " ORDER BY b.blockNo ASC LIMIT 1")
  Optional<Block> findFirstShellyBlock();

  @Query(
      value =
          """
          select b.blockNo as blockNo, ph.view as poolView, po.poolName as poolName, po.tickerName as poolTicker, sl.description as description
          from Block b
          left join SlotLeader sl on sl.id = b.slotLeaderId
          left join PoolHash ph on ph.id = sl.poolHashId
          left join PoolOfflineData po on ph.id = po.poolId AND po.id = (SELECT max(po2.id) FROM PoolOfflineData po2 WHERE po2.poolId = ph.id)
          where b.blockNo = :blockNo
      """)
  PoolMintBlockProjection getPoolInfoThatMintedBlock(@Param("blockNo") Long blockNo);
}
