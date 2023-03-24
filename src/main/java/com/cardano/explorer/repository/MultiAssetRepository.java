package com.cardano.explorer.repository;

import com.cardano.explorer.projection.AddressTokenProjection;
import com.sotatek.cardano.common.entity.Address;
import com.sotatek.cardano.common.entity.MultiAsset;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface MultiAssetRepository extends JpaRepository<MultiAsset, Long> {

  Page<MultiAsset> findAll(Pageable pageable);

  Optional<MultiAsset> findByFingerprint(String fingerprint);

  @Query("SELECT token.address.id AS addressId,"
      + " sum(COALESCE(token.balance, 0)) AS quantity"
      + " FROM AddressToken token"
      + " WHERE token.multiAsset = :multiAsset "
      + " GROUP BY token.address.id"
      + " HAVING sum(token.balance) > 0"
      + " ORDER BY sum(token.balance) DESC")
  Page<AddressTokenProjection> findAddressByToken(MultiAsset multiAsset, Pageable pageable);

  @Query("SELECT multiAsset.fingerprint AS fingerprint,"
      + " multiAsset.name AS tokenName, multiAsset.policy AS policy,"
      + " sum(COALESCE(addressToken.balance, 0)) AS quantity"
      + " FROM AddressToken addressToken"
      + " INNER JOIN MultiAsset multiAsset ON addressToken.multiAsset = multiAsset"
      + " WHERE addressToken.address = :address"
      + " GROUP BY multiAsset.fingerprint, multiAsset.name, multiAsset.policy"
      + " HAVING sum(addressToken.balance) > 0"
      + " ORDER BY sum(addressToken.balance) DESC")
  List<AddressTokenProjection> findTokenByAddress(Address address);

  Integer countByPolicy(String policy);

  Page<MultiAsset> findAllByPolicy(String policy, Pageable pageable);

  List<MultiAsset> findAllByPolicy(String policy);

  @Query("SELECT token.address.id AS addressId,"
      + " multiAsset.fingerprint AS fingerprint,"
      + " multiAsset.name AS tokenName,"
      + " sum(COALESCE(token.balance, 0)) AS quantity"
      + " FROM AddressToken token"
      + " INNER JOIN MultiAsset multiAsset ON token.multiAsset = multiAsset"
      + " WHERE token.multiAsset IN :multiAssets "
      + " GROUP BY token.address.id, multiAsset.fingerprint, multiAsset.name"
      + " HAVING sum(token.balance) > 0"
      + " ORDER BY sum(token.balance) DESC")
  Page<AddressTokenProjection> findAddressTokenByMultiAssetIn(List<MultiAsset> multiAssets, Pageable pageable);
}
