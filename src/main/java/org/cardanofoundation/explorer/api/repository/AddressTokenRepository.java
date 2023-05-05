package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.projection.TokenVolumeProjection;
import org.cardanofoundation.explorer.consumercommon.entity.AddressToken;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset;

import java.math.BigInteger;
import java.sql.Timestamp;
import java.util.Collection;
import java.util.List;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface AddressTokenRepository extends JpaRepository<AddressToken, Long> {

  @Query(value = "SELECT DISTINCT addrToken.tx.id "
      + " FROM AddressToken addrToken "
      + " WHERE addrToken.multiAsset = :multiAsset "
      + " ORDER BY addrToken.tx.id DESC")
  List<Long> findTxsByMultiAsset(MultiAsset multiAsset, Pageable pageable);

  @Query(value = "SELECT COALESCE(sum(addrToken.balance), 0)"
      + " FROM AddressToken addrToken"
      + " WHERE addrToken.multiAsset = :multiAsset "
      + " AND addrToken.balance > 0 AND addrToken.tx.id >= :txId")
  BigInteger sumBalanceAfterTx(MultiAsset multiAsset, Long txId);

  @Query(value = "SELECT COALESCE(SUM(addrToken.balance), 0)"
      + " FROM AddressToken addrToken"
      + " WHERE addrToken.multiAsset = :multiAsset"
      + " AND addrToken.tx.id >"
      + "   (SELECT MAX(tx.id) FROM Tx tx WHERE tx.blockId = "
      + "     (SELECT MAX(block.id) FROM Block block WHERE block.time < :from AND block.txCount > 0)"
      + "   )"
      + " AND addrToken.tx.id <="
      + "   (SELECT MAX(tx.id) FROM Tx tx WHERE tx.blockId = "
      + "     (SELECT MAX(block.id) FROM Block block WHERE block.time < :to AND block.txCount > 0)"
      + "   )"
      + " AND addrToken.balance > 0")
  BigInteger sumBalanceBetweenTx(MultiAsset multiAsset, Timestamp from, Timestamp to);

  @Query(value = "SELECT addrToken.multiAsset.id AS ident, "
      + " COALESCE(SUM(addrToken.balance), 0) AS volume"
      + " FROM AddressToken addrToken"
      + " WHERE addrToken.multiAsset IN :multiAsset "
      + " AND addrToken.balance > 0 AND addrToken.tx.id >= :txId"
      + " GROUP BY addrToken.multiAsset")
  List<TokenVolumeProjection> sumBalanceAfterTx(Collection<MultiAsset> multiAsset, Long txId);
}
