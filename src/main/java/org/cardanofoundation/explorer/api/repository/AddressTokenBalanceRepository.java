package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.api.projection.TokenNumberHoldersProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Address;
import org.cardanofoundation.explorer.consumercommon.entity.AddressTokenBalance;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface AddressTokenBalanceRepository extends JpaRepository<AddressTokenBalance, Long> {

  @Query("SELECT COUNT(atb.addressId) FROM AddressTokenBalance atb "
      + "INNER JOIN Address addr ON addr.id = atb.addressId "
      + "WHERE atb.multiAsset = :multiAsset "
      + "AND addr.stakeAddressId IS NULL AND atb.balance > 0 ")
  Optional<Long> countAddressNotHaveStakeByMultiAsset(@Param("multiAsset") MultiAsset multiAsset);

  @Query("SELECT COUNT(DISTINCT addr.stakeAddressId) FROM AddressTokenBalance atb "
      + "INNER JOIN Address addr ON addr.id = atb.addressId "
      + "WHERE atb.multiAsset = :multiAsset "
      + "AND atb.balance > 0 ")
  Optional<Long> countStakeByMultiAsset(@Param("multiAsset") MultiAsset multiAsset);


  @Query("SELECT COUNT(atb.addressId) as numberOfHolders, atb.multiAsset.id as ident "
      + "FROM AddressTokenBalance atb "
      + "INNER JOIN Address addr ON addr.id = atb.addressId "
      + "WHERE atb.multiAsset IN :multiAssets "
      + "AND addr.stakeAddressId IS NULL AND atb.balance > 0 "
      + "GROUP BY atb.multiAsset.id")
  List<TokenNumberHoldersProjection> countAddressNotHaveStakeByMultiAssetIn(@Param("multiAssets") List<MultiAsset> multiAssets);

  @Query("SELECT COUNT(DISTINCT addr.stakeAddressId) as numberOfHolders, atb.multiAsset.id as ident "
      + "FROM AddressTokenBalance atb "
      + "INNER JOIN Address addr ON addr.id = atb.addressId "
      + "WHERE atb.multiAsset IN :multiAssets "
      + "AND atb.balance > 0 "
      + "GROUP BY atb.multiAsset.id")
  List<TokenNumberHoldersProjection> countByMultiAssetIn(@Param("multiAssets") List<MultiAsset> multiAssets);

  @Query("SELECT atb.addressId as addressId, atb.balance as quantity"
      + " FROM AddressTokenBalance atb "
      + " WHERE atb.multiAsset = :multiAsset"
      + " ORDER BY atb.balance DESC")
  Page<AddressTokenProjection> findAddressAndBalanceByMultiAsset(@Param("multiAsset") MultiAsset multiAsset,
      Pageable pageable);

  @Query("SELECT ma.fingerprint as fingerprint, "
      + " ma.policy as policy, "
      + " ma.name as tokenName, "
      + " atb.balance as quantity"
      + " FROM AddressTokenBalance atb "
      + " INNER JOIN MultiAsset ma ON ma.id = atb.multiAsset.id"
      + " WHERE atb.address = :address"
      + " ORDER BY atb.balance DESC")
  List<AddressTokenProjection> findAddressAndBalanceByAddress(@Param("address") Address address);

  @Query("SELECT ma.fingerprint as fingerprint, ma.id as multiAssetId,"
      + " ma.policy as policy, "
      + " ma.name as tokenName, "
      + " atb.balance as quantity"
      + " FROM AddressTokenBalance atb "
      + " INNER JOIN MultiAsset ma ON ma.id = atb.multiAsset.id"
      + " WHERE atb.address = :address"
      + " ORDER BY atb.balance DESC")
  Page<AddressTokenProjection> findAddressAndBalanceByAddress(@Param("address") Address address, Pageable pageable);

  @Query("SELECT ma.name as tokenName, ma.fingerprint as fingerprint,"
      + " atb.addressId as addressId, atb.balance as quantity"
      + " FROM AddressTokenBalance atb "
      + " INNER JOIN MultiAsset ma ON ma.id = atb.multiAsset.id"
      + " WHERE ma.policy = :policy"
      + " ORDER BY atb.balance DESC")
  Page<AddressTokenProjection> findAddressAndBalanceByMultiAssetIn(@Param("policy") String policy,
      Pageable pageable);

}
