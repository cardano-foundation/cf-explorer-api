package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.Collection;
import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.model.response.script.projection.SmartContractTxProjection;
import org.cardanofoundation.explorer.api.projection.TxIOProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.Block;
import org.cardanofoundation.explorer.common.entity.ledgersync.Tx;
import org.cardanofoundation.explorer.common.entity.ledgersync.Tx_;

public interface TxRepository extends JpaRepository<Tx, Long>, JpaSpecificationExecutor<Tx> {

  @Query(
      value = "SELECT tx FROM Tx tx WHERE tx.blockId IS NOT NULL",
      countQuery = "SELECT sum(e.txCount) FROM Epoch e")
  Page<Tx> findAllTx(Pageable pageable);

  List<Tx> findByBlockIn(@Param("blocks") List<Block> blocks);

  List<Tx> findAllByBlock(@Param("block") Block block);

  @Query(
      "SELECT tx FROM Tx tx INNER JOIN Block b ON b.id = tx.blockId "
          + "WHERE b.blockNo = :blockNo")
  Page<Tx> findByBlockNo(@Param("blockNo") Long blockNo, Pageable pageable);

  @Query(
      "SELECT tx FROM Tx tx INNER JOIN Block b ON b.id = tx.blockId " + "WHERE b.hash = :blockHash")
  Page<Tx> findByBlockHash(@Param("blockHash") String blockHash, Pageable pageable);

  @EntityGraph(attributePaths = {Tx_.BLOCK, Tx_.TX_METADATA_HASH})
  Optional<Tx> findByHash(@Param("hash") String hash);

  @Query(value = "SELECT tx.id FROM Tx tx ")
  List<Long> findLatestTxId(Pageable pageable);

  @Query(
      value =
          "SELECT tx.hash as hash, "
              + "b.blockNo as blockNo, "
              + "outp.address as toAddress, "
              + "inp.address as fromAddress, "
              + "tx.outSum as amount,"
              + "tx.validContract as validContract, "
              + "b.time as time, "
              + "b.epochNo as epochNo, "
              + "b.epochSlotNo as epochSlotNo, "
              + "b.slotNo as slot "
              + "FROM Tx tx "
              + "JOIN Block b ON b.id = tx.blockId "
              + "JOIN TxIn txi ON txi.txInputId = tx.id "
              + "LEFT JOIN TxOut outp ON outp.tx.id = tx.id "
              + "LEFT JOIN TxOut inp ON inp.tx.id = txi.txOutputId AND "
              + "inp.index = txi.txOutIndex "
              + "WHERE tx.id IN :txIds "
              + "ORDER BY b.blockNo DESC, tx.blockIndex DESC")
  List<TxIOProjection> findLatestTxIO(@Param("txIds") Collection<Long> txIds);

  @Query("SELECT tx FROM Tx tx WHERE tx.id IN :ids ORDER BY tx.blockId DESC, tx.blockIndex DESC")
  @EntityGraph(attributePaths = {Tx_.BLOCK})
  List<Tx> findByIdIn(@Param("ids") List<Long> ids);

  @Query(
      "SELECT tx.id as id, "
          + "tx.hash as hash, "
          + "b.blockNo as blockNo, "
          + "b.time as time, "
          + "b.epochNo as epochNo, "
          + "b.epochSlotNo as epochSlotNo, "
          + "b.slotNo as slot "
          + "FROM Tx tx "
          + "JOIN Block b ON b.id = tx.blockId "
          + "WHERE tx.id IN :txIds "
          + "ORDER BY b.blockNo DESC, tx.blockIndex DESC")
  List<TxIOProjection> findTxIn(@Param("txIds") Collection<Long> txIds);

  @Query(
      "SELECT tx.id as txId, tx.hash as hash, b.time as time, b.blockNo as blockNo,  "
          + " b.epochNo as epochNo, b.epochSlotNo as epochSlotNo, b.slotNo as absoluteSlot "
          + " FROM Tx tx "
          + " JOIN Block b ON tx.block = b"
          + " WHERE tx.id IN :txIds")
  List<SmartContractTxProjection> getSmartContractTxsByTxIds(
      @Param("txIds") Collection<Long> txIds);

  @Query(
      "SELECT DISTINCT(r.purpose) as scriptPurposeType, tx.id as txId FROM Tx tx"
          + " JOIN Redeemer r ON r.tx = tx"
          + " WHERE tx.id IN :txIds"
          + " AND r.scriptHash = :scriptHash")
  List<SmartContractTxProjection> getSmartContractTxsPurpose(
      @Param("txIds") Collection<Long> txIds, @Param("scriptHash") String scriptHash);

  List<Tx> findAllByHashIn(List<String> txHashes);
}
