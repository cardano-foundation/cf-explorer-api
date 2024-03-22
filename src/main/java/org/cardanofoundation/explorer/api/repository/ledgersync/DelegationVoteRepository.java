package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.api.projection.DRepDelegatorProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.DelegationVote;
import org.cardanofoundation.explorer.common.entity.ledgersync.compositeKey.DelegationVoteId;

@Repository
public interface DelegationVoteRepository extends JpaRepository<DelegationVote, DelegationVoteId> {

  @Query(
      value =
          "select dv.address as stakeAddress, dv.txHash as txHash, dv.blockTime as blockTime, t.fee as fee "
              + " from DelegationVote dv"
              + " join Tx t on t.hash = dv.txHash"
              + " where dv.drepHash = :dRepHashOrDRepId or dv.drepId = :dRepHashOrDRepId")
  Page<DRepDelegatorProjection> getDelegationVoteByDrepHashOrDRepId(
      @Param("dRepHashOrDRepId") String dRepHashOrDRepId, Pageable pageable);
}
