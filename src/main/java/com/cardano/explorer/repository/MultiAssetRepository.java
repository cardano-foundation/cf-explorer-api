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

  @Query("SELECT output.id.address AS address,"
      + " (output.quantity - COALESCE(input.quantity, 0)) AS quantity"
      + " FROM AddressTokenOutput output"
      + " LEFT JOIN AddressTokenInput input"
      + " ON output.id.address = input.id.address"
      + " AND output.id.fingerprint = input.id.fingerprint"
      + " WHERE output.id.fingerprint = :fingerprint AND (output.quantity - COALESCE(input.quantity, 0)) > 0 "
      + " ORDER BY (output.quantity - COALESCE(input.quantity, 0)) DESC")
  Page<AddressTokenProjection> findAddressByToken(String fingerprint, Pageable pageable);

  @Query("SELECT output.id.fingerprint AS fingerprint,"
      + " output.tokenName AS tokenName,"
      + " (output.quantity - COALESCE(input.quantity, 0)) AS quantity"
      + " FROM AddressTokenOutput output"
      + " LEFT JOIN AddressTokenInput input"
      + " ON output.id.fingerprint = input.id.fingerprint"
      + " AND output.id.address = input.id.address"
      + " WHERE output.id.address = :address AND (output.quantity - COALESCE(input.quantity, 0)) > 0"
      + " ORDER BY (output.quantity - COALESCE(input.quantity, 0)) DESC")
  List<AddressTokenProjection> findTokenByAddress(String address);
}
