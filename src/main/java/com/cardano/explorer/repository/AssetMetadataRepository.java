package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.AssetMetadata;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;

public interface AssetMetadataRepository extends JpaRepository<AssetMetadata, Long> {

  Optional<AssetMetadata> findFirstBySubject(String subject);
}
