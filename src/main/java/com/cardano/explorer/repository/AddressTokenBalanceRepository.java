package com.cardano.explorer.repository;

import com.cardano.explorer.projection.AddressTokenProjection;
import com.cardano.explorer.projection.TokenNumberHoldersProjection;
import com.sotatek.cardano.common.entity.AddressTokenBalance;
import com.sotatek.cardano.common.entity.MultiAsset;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface AddressTokenBalanceRepository extends JpaRepository<AddressTokenBalance, Long> {

  Optional<Long> countByMultiAsset(MultiAsset multiAsset);


  @Query("SELECT COUNT(atb) as numberOfHolders, atb.multiAsset.id as ident"
      + " FROM AddressTokenBalance atb "
      + " WHERE atb.multiAsset IN :multiAssets AND atb.balance > 0"
      + " GROUP BY atb.multiAsset.id")
  List<TokenNumberHoldersProjection> countByMultiAssetIn(List<MultiAsset> multiAssets);

  @Query("SELECT atb.addressId as addressId, atb.balance as quantity"
      + " FROM AddressTokenBalance atb "
      + " WHERE atb.multiAsset = :multiAsset"
      + " ORDER BY atb.balance DESC")
  Page<AddressTokenProjection> findAddressAndBalanceByMultiAsset(MultiAsset multiAsset,
      Pageable pageable);

  @Query("SELECT atb.addressId as addressId, atb.balance as quantity"
      + " FROM AddressTokenBalance atb "
      + " WHERE atb.multiAsset IN :multiAssets"
      + " ORDER BY atb.balance DESC")
  Page<AddressTokenProjection> findAddressAndBalanceByMultiAsset(Collection<MultiAsset> multiAssets,
      Pageable pageable);

}
