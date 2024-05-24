// package org.cardanofoundation.explorer.api.repository.ledgersync;
//
// import java.math.BigInteger;
// import java.sql.Timestamp;
// import java.util.Collection;
// import java.util.List;
// import java.util.Optional;
//
// import org.springframework.data.domain.Pageable;
// import org.springframework.data.jpa.repository.JpaRepository;
// import org.springframework.data.jpa.repository.Query;
// import org.springframework.data.repository.query.Param;
//
// import org.cardanofoundation.explorer.common.entity.ledgersync.AddressToken;
// import org.cardanofoundation.explorer.common.entity.ledgersync.MultiAsset;
//
// public interface AddressTokenRepository extends JpaRepository<AddressToken, Long> {
//
//  @Query(
//      value =
//          "SELECT DISTINCT addrToken.tx.id "
//              + " FROM AddressToken addrToken "
//              + " WHERE addrToken.multiAsset = :multiAsset "
//              + " ORDER BY addrToken.tx.id DESC")
//  List<Long> findTxsByMultiAsset(@Param("multiAsset") MultiAsset multiAsset, Pageable pageable);
//
//  @Query(
//      value =
//          "SELECT COALESCE(SUM(addrToken.balance), 0)"
//              + " FROM AddressToken addrToken"
//              + " WHERE addrToken.multiAsset = :multiAsset"
//              + " AND addrToken.tx.id >"
//              + "   (SELECT MAX(tx.id) FROM Tx tx WHERE tx.blockId = "
//              + "     (SELECT MAX(block.id) FROM Block block WHERE block.time < :from AND
// block.txCount > 0)"
//              + "   )"
//              + " AND addrToken.tx.id <="
//              + "   (SELECT MAX(tx.id) FROM Tx tx WHERE tx.blockId = "
//              + "     (SELECT MAX(block.id) FROM Block block WHERE block.time < :to AND
// block.txCount > 0)"
//              + "   )"
//              + " AND addrToken.balance > 0")
//  Optional<BigInteger> sumBalanceBetweenTx(
//      @Param("multiAsset") MultiAsset multiAsset,
//      @Param("from") Timestamp from,
//      @Param("to") Timestamp to);
//
//  @Query(
//      "SELECT addrToken FROM AddressToken addrToken"
//          + " WHERE addrToken.tx.id in :ids and addrToken.address.address = :address")
//  List<AddressToken> findByTxIdInAndByAddress(
//      @Param("ids") Collection<Long> ids, @Param("address") String address);
//
//  @Query(
//      "SELECT addrToken FROM AddressToken addrToken"
//          + " WHERE addrToken.tx.id in :ids and addrToken.address.stakeAddress.id = :stakeId")
//  List<AddressToken> findByTxIdInAndStakeId(
//      @Param("ids") Collection<Long> ids, @Param("stakeId") Long stakeId);
// }
