package org.cardanofoundation.explorer.api.repository;

import java.sql.Timestamp;
import java.util.Collection;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface MultiAssetRepository extends JpaRepository<MultiAsset, Long> {

  Page<MultiAsset> findAll(Pageable pageable);

  Optional<MultiAsset> findByFingerprint(@Param("fingerprint") String fingerprint);

  Integer countByPolicy(@Param("policy") String policy);

  Page<MultiAsset> findAllByPolicy(@Param("policy") String policy, Pageable pageable);

  List<MultiAsset> findAllByIdIn(@Param("ids") Collection<Long> ids);

  @Query("SELECT b.time FROM Tx tx JOIN Block b ON b.id = tx.blockId "
      + "WHERE tx.id = (SELECT max(adt.txId) FROM AddressToken adt WHERE adt.multiAsset = :multiAsset)")
  Timestamp getLastActivityTimeOfToken(@Param("multiAsset") MultiAsset multiAsset);
}