package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.common.entity.ledgersync.AssetMetadata;

public interface AssetMetadataRepository extends JpaRepository<AssetMetadata, Long> {

  Optional<AssetMetadata> findFirstBySubject(@Param("subject") String subject);

  List<AssetMetadata> findBySubjectIn(@Param("subjects") Set<String> subjects);

  List<AssetMetadata> findByFingerprintIn(@Param("fingerprints") Set<String> fingerprints);
}
