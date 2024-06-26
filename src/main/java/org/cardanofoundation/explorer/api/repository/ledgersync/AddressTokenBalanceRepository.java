// package org.cardanofoundation.explorer.api.repository.ledgersync;
//
// import java.util.List;
// import java.util.Optional;
//
// import org.springframework.data.domain.Page;
// import org.springframework.data.domain.Pageable;
// import org.springframework.data.jpa.repository.JpaRepository;
// import org.springframework.data.jpa.repository.Query;
// import org.springframework.data.repository.query.Param;
//
// import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
// import org.cardanofoundation.explorer.common.entity.ledgersync.Address;
// import org.cardanofoundation.explorer.common.entity.ledgersync.AddressTokenBalance;
// import org.cardanofoundation.explorer.common.entity.ledgersync.MultiAsset;
//
// public interface AddressTokenBalanceRepository extends JpaRepository<AddressTokenBalance, Long> {
//
////  @Query(
////      "SELECT COUNT(atb.addressId) FROM AddressTokenBalance atb "
////          + "WHERE atb.multiAsset = :multiAsset "
////          + "AND atb.stakeAddress.id IS NULL AND atb.balance > 0 ")
////  Optional<Long> countAddressNotHaveStakeByMultiAsset(@Param("multiAsset") MultiAsset
// multiAsset);
////
////  @Query(
////      "SELECT COUNT(DISTINCT atb.stakeAddress.id) FROM AddressTokenBalance atb "
////          + "WHERE atb.multiAsset = :multiAsset "
////          + "AND atb.balance > 0 ")
////  Optional<Long> countStakeByMultiAsset(@Param("multiAsset") MultiAsset multiAsset);
////
////  @Query(
////      value =
////          "SELECT COALESCE(atb.stakeAddress.id, atb.addressId * -1L) as addressId,
//// SUM(atb.balance) as quantity"
////              + " FROM AddressTokenBalance atb "
////              + " WHERE atb.multiAsset = :multiAsset"
////              + " AND atb.balance > 0"
////              + " GROUP BY COALESCE(atb.stakeAddress.id, atb.addressId * -1L)"
////              + " ORDER BY SUM(atb.balance) DESC")
////  List<AddressTokenProjection> findAddressAndBalanceByMultiAsset(
////      @Param("multiAsset") MultiAsset multiAsset, Pageable pageable);
////
////  @Query(
////      "SELECT ma.fingerprint as fingerprint, "
////          + " ma.policy as policy, "
////          + " ma.name as tokenName, "
////          + " atb.balance as quantity"
////          + " FROM AddressTokenBalance atb "
////          + " INNER JOIN MultiAsset ma ON ma.id = atb.multiAsset.id"
////          + " WHERE atb.address = :address"
////          + " AND (lower(ma.nameView) LIKE CONCAT('%', :searchValue, '%') OR fingerprint =
//// :searchValue)"
////          + " AND atb.balance > 0")
////  Page<AddressTokenProjection> findTokenAndBalanceByAddressAndNameView(
////      @Param("address") Address address,
////      @Param("searchValue") String searchValue,
////      Pageable pageable);
////
////  @Query(
////      "SELECT ma.fingerprint as fingerprint, ma.id as multiAssetId,"
////          + " ma.policy as policy, "
////          + " ma.name as tokenName, "
////          + " atb.balance as quantity,"
////          + " am.url as url, am.decimals as decimals, am.ticker as ticker,"
////          + " am.logo as logo, am.description as description, am.subject as subject"
////          + " FROM AddressTokenBalance atb "
////          + " INNER JOIN MultiAsset ma ON ma.id = atb.multiAsset.id"
////          + " LEFT JOIN AssetMetadata am ON am.fingerprint=ma.fingerprint"
////          + " WHERE atb.address = :address"
////          + " AND atb.balance > 0")
////  Page<AddressTokenProjection> findTokenAndBalanceByAddress(
////      @Param("address") Address address, Pageable pageable);
////
////  @Query(
////      "SELECT ma.name as tokenName, ma.fingerprint as fingerprint,"
////          + " atb.addressId as addressId, atb.balance as quantity, ma.policy as policy"
////          + " FROM AddressTokenBalance atb "
////          + " INNER JOIN MultiAsset ma ON ma.id = atb.multiAsset.id"
////          + " WHERE ma.policy = :policy"
////          + " AND atb.balance > 0"
////          + " ORDER BY atb.balance DESC")
////  Page<AddressTokenProjection> findAddressAndBalanceByMultiAssetIn(
////      @Param("policy") String policy, Pageable pageable);
//
//  @Query(
//      value = """
//
//  SELECT ltb.address as address, ltb.quantity as balance, ma.nameView as tokenName, ma.fingerprint
// as fingerprint
//            FROM LatestTokenBalance ltb
//            inner JOIN MultiAsset ma ON ma.unit = ltb.unit
//            WHERE ma.policy = :policy
//        order by ltb.quantity desc
//       """)
//  List<AddressTokenProjection> findAddressAndBalanceByPolicy(
//      @Param("policy") String policy, Pageable pageable);
// }
