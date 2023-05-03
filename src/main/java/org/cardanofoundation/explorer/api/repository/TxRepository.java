package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.projection.TxGraphProjection;
import org.cardanofoundation.explorer.api.projection.TxIOProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Block;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import org.cardanofoundation.explorer.consumercommon.entity.Tx_;
import java.sql.Timestamp;
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

public interface TxRepository extends JpaRepository<Tx, Long>, JpaSpecificationExecutor<Tx> {

  @Query(value = "SELECT tx FROM Tx tx WHERE tx.blockId IS NOT NULL"
      , countQuery = "SELECT sum(e.txCount) FROM Epoch e")
  Page<Tx> findAllTx(Pageable pageable);

  List<Tx> findByBlockIn(List<Block> blocks);

  @Query("SELECT tx FROM Tx tx INNER JOIN Block b ON b.id = tx.blockId "
      + "WHERE b.blockNo = :blockNo")
  Page<Tx> findByBlockNo(Long blockNo, Pageable pageable);

  @Query("SELECT tx FROM Tx tx INNER JOIN Block b ON b.id = tx.blockId "
      + "WHERE b.hash = :blockHash")
  Page<Tx> findByBlockHash(String blockHash, Pageable pageable);

  @EntityGraph(attributePaths = {Tx_.BLOCK})
  Optional<Tx> findByHash(String hash);

  @Query(value = "SELECT tx.id FROM Tx tx ")
  List<Long> findLatestTxId(Pageable pageable);


  @Query(value = "SELECT tx.hash as hash, "
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
  List<TxIOProjection> findLatestTxIO(Collection<Long> txIds);

  @Query(value = "SELECT b.time as time, b.txCount as transactionNo "
      + "FROM Block b "
      + "WHERE b.time >= :time "
      + "ORDER BY b.time DESC")
  List<TxGraphProjection> getTransactionsAfterTime(@Param("time") Timestamp time);

  @Query("SELECT tx FROM Tx tx WHERE tx.id IN :ids ORDER BY tx.blockId DESC, tx.blockIndex DESC")
  List<Tx> findByIdIn(List<Long> ids);

  @Query("SELECT min(tx.id) FROM Tx tx "
      + " INNER JOIN Block b ON b.id = tx.blockId"
      + " WHERE b.time >= :time AND b.txCount > 0")
  Optional<Long> findMinTxByAfterTime(@Param("time") Timestamp time);
}
