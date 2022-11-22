package com.cardano.explorer.repository;

import com.cardano.explorer.projection.TxIOProjection;
import com.sotatek.cardano.common.entity.Block;
import com.sotatek.cardano.common.entity.Tx;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

public interface TxRepository extends JpaRepository<Tx, Long>, JpaSpecificationExecutor<Tx> {

  List<Tx> findByBlockIn(List<Block> blocks);

  @EntityGraph(attributePaths = {"block"})
  Optional<Tx> findByHash(String hash);

  @Query(value = "SELECT tx.id FROM Tx tx ORDER BY tx.id DESC ")
  Page<Long> findLatestTxId(Pageable pageable);


  @Query(value = "SELECT tx.hash as hash, "
      + "b.blockNo as blockNo, "
      + "outp.address as toAddress, "
      + "inp.address as fromAddress, "
      + "outp.value as amount "
      + "FROM Tx tx "
      + "JOIN Block b ON b.id = tx.blockId "
      + "JOIN TxIn txi ON txi.txInputId = tx.id "
      + "LEFT JOIN TxOut outp ON outp.tx.id = tx.id "
      + "LEFT JOIN TxOut inp ON inp.tx.id = txi.txOutputId AND "
      + "inp.index = txi.txOutIndex "
      + "WHERE tx.id IN :txIds "
      + "ORDER BY b.blockNo DESC, txi.txOutputId DESC")
  List<TxIOProjection> findLatestTxIO(Collection<Long> txIds);

}
