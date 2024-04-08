package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.api.projection.DRepDelegatorProjection;
import org.cardanofoundation.explorer.common.entity.compositeKey.DelegationVoteId;
import org.cardanofoundation.explorer.common.entity.ledgersync.DelegationVote;

@Repository
public interface DelegationVoteRepository extends JpaRepository<DelegationVote, DelegationVoteId> {

  @Query(
      value =
          "select dv.address as stakeAddress, dv.txHash as txHash, dv.blockTime as blockTime, t.fee as fee "
              + " from DelegationVote dv"
              + " join Tx t on t.hash = dv.txHash"
              + " where (dv.drepHash = :dRepHashOrDRepId or dv.drepId = :dRepHashOrDRepId)"
              + " and not exists"
              + " (select true from DelegationVote dv2"
              + " where dv2.drepHash = dv.drepHash"
              + " and dv2.address = dv.address"
              + " and dv2.slot > dv.slot)")
  Page<DRepDelegatorProjection> getDelegationVoteByDRepHashOrDRepId(
      @Param("dRepHashOrDRepId") String dRepHashOrDRepId, Pageable pageable);
}
