package org.cardanofoundation.explorer.api.repository;

import java.util.Collection;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.query.Param;

public interface MultiAssetRepository extends JpaRepository<MultiAsset, Long> {

  Page<MultiAsset> findAll(Pageable pageable);

  Optional<MultiAsset> findByFingerprint(String fingerprint);

  Integer countByPolicy(String policy);

  Page<MultiAsset> findAllByPolicy(String policy, Pageable pageable);

  List<MultiAsset> findAllByPolicy(String policy);

  List<MultiAsset> findAllByIdIn(@Param("ids") Collection<Long> ids);
}
