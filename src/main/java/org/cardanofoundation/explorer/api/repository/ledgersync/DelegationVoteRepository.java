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
          """
      SELECT t.id as id, dv.address as stakeAddress, dv.txHash as txHash, dv.blockTime as blockTime, t.fee as fee
                    FROM DelegationVote dv
                    LEFT JOIN Tx t ON t.hash = dv.txHash
                    WHERE (dv.drepId = :dRepHashOrDRepId OR dv.drepHash = :dRepHashOrDRepId)
                    AND NOT EXISTS
                    (SELECT true FROM DelegationVote dv2
                    LEFT JOIN Tx t1 ON t1.hash = dv2.txHash
                    WHERE dv2.drepHash = dv.drepHash
                    AND dv2.address = dv.address
                    AND t1.id > t.id)""")
  Page<DRepDelegatorProjection> getDelegationVoteByDRepHashOrDRepId(
      @Param("dRepHashOrDRepId") String dRepHashOrDRepId, Pageable pageable);
}
