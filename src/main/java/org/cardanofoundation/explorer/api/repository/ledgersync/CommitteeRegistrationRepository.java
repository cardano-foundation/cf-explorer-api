package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.VotingCCProjection;
import org.cardanofoundation.explorer.common.entity.compositeKey.CommitteeRegistrationId;
import org.cardanofoundation.explorer.common.entity.enumeration.VoterType;
import org.cardanofoundation.explorer.common.entity.ledgersync.CommitteeRegistration;

public interface CommitteeRegistrationRepository
    extends JpaRepository<CommitteeRegistration, CommitteeRegistrationId> {
  @Query(
      value =
          "SELECT COUNT(cr) FROM CommitteeRegistration cr "
              + "JOIN EpochParam ep ON cr.epoch = ep.epochNo "
              + "WHERE (ep.epochNo + ep.committeeMaxTermLength) >= :expiredEpoch")
  Integer countByExpiredEpochNo(@Param("expiredEpoch") Integer expiredEpoch);

  @Query(
      value =
          """
  select lvp.voterHash as voterHash, lvp.txHash as txHash, lvp.index as index, lvp.vote as vote from CommitteeRegistration cr
  left join LatestVotingProcedure lvp on lvp.govActionTxHash = :txHash
  and lvp.govActionIndex = :index and lvp.voterType = :voterType and lvp.voterHash = cr.hotKey
  where not exists ( select 1 from CommitteeRegistration cr2 where cr2.coldKey = cr.coldKey and cr2.blockTime > cr.blockTime)
  and not exists (select 1 from CommitteeDeRegistration cd where cd.coldKey = cr.coldKey and cd.blockTime > cr.blockTime)
""")
  List<VotingCCProjection> findByTxHashAndIndex(
      @Param("txHash") String txHash,
      @Param("index") Integer index,
      @Param("voterType") List<VoterType> voterType);
}
