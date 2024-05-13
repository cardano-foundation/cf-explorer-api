package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.common.entity.ledgersync.CommitteeInfo;

public interface CommitteInfoRepository extends JpaRepository<CommitteeInfo, String> {
  @Query(value = """
  select count(*) from CommitteeInfo ci
  where ci.createdAt <= :createdAt
""")
  Long countByCreatedAtLessThan(@Param("createdAt") Long createdAt);
}
