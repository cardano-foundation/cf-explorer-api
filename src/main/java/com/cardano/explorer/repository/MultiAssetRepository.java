package com.cardano.explorer.repository;

import com.cardano.explorer.projection.AddressTokenProjection;
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

  @Query("SELECT addr.address AS address,"
      + " sum(COALESCE(token.balance, 0)) AS quantity"
      + " FROM AddressToken token"
      + " INNER JOIN Address addr ON token.address = addr"
      + " WHERE token.multiAsset.fingerprint = :fingerprint "
      + " GROUP BY addr.address"
      + " HAVING sum(token.balance) > 0"
      + " ORDER BY sum(token.balance) DESC")
  Page<AddressTokenProjection> findAddressByToken(String fingerprint, Pageable pageable);

  @Query("SELECT multiAsset.fingerprint AS fingerprint,"
      + " multiAsset.name AS tokenName,"
      + " sum(COALESCE(addressToken.balance, 0)) AS quantity"
      + " FROM AddressToken addressToken"
      + " INNER JOIN MultiAsset multiAsset ON addressToken.multiAsset = multiAsset"
      + " INNER JOIN Address addr ON addressToken.address = addr"
      + " WHERE addr.address = :address "
      + " GROUP BY multiAsset.fingerprint, multiAsset.name"
      + " HAVING sum(addressToken.balance) > 0"
      + " ORDER BY sum(addressToken.balance) DESC")
  List<AddressTokenProjection> findTokenByAddress(String address);

  Integer countByPolicy(String policy);

  Page<MultiAsset> findAllByPolicy(String policy, Pageable pageable);

  @Query("SELECT addr.address AS address,"
      + " multiAsset.fingerprint AS fingerprint,"
      + " multiAsset.name AS tokenName,"
      + " sum(COALESCE(token.balance, 0)) AS quantity"
      + " FROM AddressToken token"
      + " INNER JOIN MultiAsset multiAsset ON token.multiAsset = multiAsset"
      + " INNER JOIN Address addr ON token.address = addr"
      + " WHERE multiAsset.policy = :policy "
      + " GROUP BY addr.address, multiAsset.fingerprint, multiAsset.name"
      + " HAVING sum(token.balance) > 0"
      + " ORDER BY sum(token.balance) DESC")
  Page<AddressTokenProjection> findAddressTokenByPolicy(String policy, Pageable pageable);
}
