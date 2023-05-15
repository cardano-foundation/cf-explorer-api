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
import org.springframework.data.repository.query.Param;

public interface AddressTokenRepository extends JpaRepository<AddressToken, Long> {

  @Query(value = "SELECT DISTINCT addrToken.tx.id "
      + " FROM AddressToken addrToken "
      + " WHERE addrToken.multiAsset = :multiAsset "
      + " ORDER BY addrToken.tx.id DESC")
  List<Long> findTxsByMultiAsset(@Param("multiAsset") MultiAsset multiAsset, Pageable pageable);

  @Query(value = "SELECT COALESCE(sum(addrToken.balance), 0)"
      + " FROM AddressToken addrToken"
      + " WHERE addrToken.multiAsset = :multiAsset "
      + " AND addrToken.balance > 0 AND addrToken.tx.id >= :txId")
  BigInteger sumBalanceAfterTx(@Param("multiAsset") MultiAsset multiAsset, @Param("txId") Long txId);

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
  BigInteger sumBalanceBetweenTx(@Param("multiAsset") MultiAsset multiAsset,
                                 @Param("from") Timestamp from, @Param("to") Timestamp to);

  @Query(value = "SELECT addrToken.multiAsset.id AS ident, "
      + " COALESCE(SUM(addrToken.balance), 0) AS volume"
      + " FROM AddressToken addrToken"
      + " WHERE addrToken.multiAsset IN :multiAsset "
      + " AND addrToken.balance > 0 AND addrToken.tx.id >= :txId"
      + " GROUP BY addrToken.multiAsset")
  List<TokenVolumeProjection> sumBalanceAfterTx(
      @Param("multiAsset") Collection<MultiAsset> multiAsset, @Param("txId") Long txId);

  @Query("SELECT addrToken FROM AddressToken addrToken"
      + " WHERE addrToken.tx.id in :ids and addrToken.address.address = :address")
  List<AddressToken> findByTxIdInAndByAddress(@Param("ids") Collection<Long> ids,
                                              @Param("address") String address);
}
