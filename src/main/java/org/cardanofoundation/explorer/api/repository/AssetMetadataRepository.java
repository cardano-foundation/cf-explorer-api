package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.consumercommon.entity.AssetMetadata;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import org.springframework.data.jpa.repository.JpaRepository;

public interface AssetMetadataRepository extends JpaRepository<AssetMetadata, Long> {

  Optional<AssetMetadata> findFirstBySubject(String subject);
  List<AssetMetadata> findBySubjectIn(Set<String> subjects);
}
