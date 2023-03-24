package com.cardano.explorer.repository;

import com.cardano.explorer.projection.AddressTokenProjection;
import com.sotatek.cardano.common.entity.Address;
import com.sotatek.cardano.common.entity.MultiAsset;
import java.util.List;
import java.util.Optional;
import java.util.Set;
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

  @Query("SELECT at.multiAsset.id as multiAssetId, sum(at.balance) as quantity from AddressToken at"
      + " WHERE at.address = :address"
      + " GROUP BY at.multiAsset.id"
      + " ORDER BY sum(at.balance) DESC")
  Page<AddressTokenProjection> getIdentListByAddress(Address address, Pageable pageable);

  @Query("SELECT at.multiAsset.id as multiAssetId, sum(at.balance) as quantity from AddressToken at"
      + " WHERE at.address = :address"
      + " GROUP BY at.multiAsset.id"
      + " ORDER BY sum(at.balance) DESC")
  List<AddressTokenProjection> getIdentListByAddress(Address address);

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
