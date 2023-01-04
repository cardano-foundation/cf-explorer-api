package com.cardano.explorer.repository;

import com.cardano.explorer.projection.AddressTokenProjection;
import com.sotatek.cardano.common.entity.MultiAsset;
import com.sotatek.cardano.common.entity.MultiAsset_;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface MultiAssetRepository extends JpaRepository<MultiAsset, Long> {

  @EntityGraph(attributePaths = {MultiAsset_.METADATA})
  Page<MultiAsset> findAll(Pageable pageable);

  @EntityGraph(attributePaths = {MultiAsset_.METADATA})
  Optional<MultiAsset> findByFingerprint(String fingerprint);

  @Query("SELECT token.address AS address,"
      + " sum(COALESCE(token.balance, 0)) AS quantity"
      + " FROM AddressToken token"
      + " WHERE token.multiAsset.fingerprint = :fingerprint "
      + " GROUP BY token.address"
      + " HAVING sum(token.balance) > 0"
      + " ORDER BY sum(token.balance) DESC")
  Page<AddressTokenProjection> findAddressByToken(String fingerprint, Pageable pageable);

  @Query("SELECT token.multiAsset.fingerprint AS fingerprint,"
      + " token.multiAsset.name AS tokenName,"
      + " sum(COALESCE(token.balance, 0)) AS quantity"
      + " FROM AddressToken token"
      + " WHERE token.address = :address "
      + " GROUP BY token.multiAsset.fingerprint, token.multiAsset.name"
      + " HAVING sum(token.balance) > 0"
      + " ORDER BY sum(token.balance) DESC")
  List<AddressTokenProjection> findTokenByAddress(String address);

  Integer countByPolicy(String policy);

  @EntityGraph(attributePaths = {MultiAsset_.METADATA})
  Page<MultiAsset> findAllByPolicy(String policy, Pageable pageable);

  @Query("SELECT token.address AS address,"
      + " multiAsset.fingerprint AS fingerprint,"
      + " multiAsset.name AS tokenName,"
      + " sum(COALESCE(token.balance, 0)) AS quantity"
      + " FROM AddressToken token"
      + " INNER JOIN MultiAsset multiAsset ON token.multiAsset = multiAsset"
      + " WHERE multiAsset.policy = :policy "
      + " GROUP BY token.address, multiAsset.fingerprint, multiAsset.name"
      + " HAVING sum(token.balance) > 0"
      + " ORDER BY sum(token.balance) DESC")
  Page<AddressTokenProjection> findAddressTokenByPolicy(String policy, Pageable pageable);
}
