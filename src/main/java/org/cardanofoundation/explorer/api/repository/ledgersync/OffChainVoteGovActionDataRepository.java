package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.common.entity.compositeKey.OffChainVoteGovActionDataId;
import org.cardanofoundation.explorer.common.entity.ledgersync.OffChainVoteGovActionData;

public interface OffChainVoteGovActionDataRepository
    extends JpaRepository<OffChainVoteGovActionData, OffChainVoteGovActionDataId> {

  @Query(
      """
    SELECT oc.rawData FROM OffChainVoteGovActionData oc
    WHERE oc.anchorUrl = :anchorUrl AND oc.anchorHash = :anchorHash
    """)
  String getRawDataByAnchorUrlAndAnchorHash(
      @Param("anchorUrl") String anchorUrl, @Param("anchorHash") String anchorHash);
}
