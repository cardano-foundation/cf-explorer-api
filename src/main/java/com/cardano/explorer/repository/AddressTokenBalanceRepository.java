package com.cardano.explorer.repository;

import com.cardano.explorer.projection.TokenNumberHoldersProjection;
import com.sotatek.cardano.common.entity.AddressTokenBalance;
import com.sotatek.cardano.common.entity.MultiAsset;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface AddressTokenBalanceRepository extends JpaRepository<AddressTokenBalance, Long> {

  Optional<Long> countByMultiAsset(MultiAsset multiAsset);


  @Query("SELECT COUNT(atb) as numberOfHolders, atb.multiAsset.id as ident"
      + " FROM AddressTokenBalance atb "
      + " WHERE atb.multiAsset IN :multiAssets"
      + " GROUP BY atb.multiAsset.id")
  List<TokenNumberHoldersProjection> countByMultiAssetIn(List<MultiAsset> multiAssets);

}
