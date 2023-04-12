package com.cardano.explorer.repository;

import com.cardano.explorer.projection.TokenVolumeProjection;
import com.sotatek.cardano.common.entity.AddressToken;
import com.sotatek.cardano.common.entity.MultiAsset;
import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.Collection;
import java.util.List;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface AddressTokenRepository extends JpaRepository<AddressToken, Long> {

  @Query(value = "SELECT DISTINCT addrToken.tx.id "
      + " FROM AddressToken addrToken "
      + " WHERE addrToken.multiAsset = :multiAsset "
      + " ORDER BY addrToken.tx.id DESC")
  List<Long> findTxsByMultiAsset(MultiAsset multiAsset, Pageable pageable);

  @Query(value = "SELECT sum(addrToken.balance)"
      + " FROM AddressToken addrToken"
      + " WHERE addrToken.multiAsset = :multiAsset "
      + " AND addrToken.balance > 0 AND addrToken.tx.id >= "
      + " (SELECT min(tx.id) FROM Tx tx "
      + " INNER JOIN Block b ON b.id = tx.blockId"
      + " WHERE b.time >= :time)")
  BigInteger sumBalanceAfterTx(MultiAsset multiAsset, Timestamp time);

  @Query(value = "SELECT sum(addrToken.balance)"
      + " FROM AddressToken addrToken"
      + " WHERE addrToken.multiAsset = :multiAsset"
      + " AND addrToken.tx.id >"
      + "   (SELECT max(tx.id) FROM Tx tx WHERE tx.blockId = "
      + "     (SELECT max(block.id) FROM Block block WHERE block.time < :from AND block.txCount > 0)"
      + "   )"
      + " AND addrToken.tx.id <="
      + "   (SELECT max(tx.id) FROM Tx tx WHERE tx.blockId = "
      + "     (SELECT max(block.id) FROM Block block WHERE block.time < :to AND block.txCount > 0)"
      + "   )"
      + " AND addrToken.balance > 0")
  BigInteger sumBalanceBetweenTx(MultiAsset multiAsset, Timestamp from, Timestamp to);

  @Query(value = "SELECT addrToken.multiAsset.id as ident, sum(addrToken.balance) as volume"
      + " FROM AddressToken addrToken"
      + " WHERE addrToken.multiAsset IN :multiAsset "
      + " AND addrToken.balance > 0 AND addrToken.tx.id >="
      + " (SELECT min(tx.id) FROM Tx tx "
      + " INNER JOIN Block b ON b.id = tx.blockId"
      + " WHERE b.time >= :time)"
      + " GROUP BY addrToken.multiAsset")
  List<TokenVolumeProjection> sumBalanceAfterTx(Collection<MultiAsset> multiAsset, Timestamp time);

  @Query("SELECT at.tx.id "
      + "FROM AddressToken at "
      + "INNER JOIN Address addr ON addr.id = at.address.id "
      + "WHERE at.tx.id IN :txIds")
  List<Long> findTransactionHaveToken(@Param("txIds") List<Long> txIds);
}
