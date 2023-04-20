package com.cardano.explorer.repository;

import com.cardano.explorer.projection.ParamChange;
import com.sotatek.cardano.common.entity.ParamProposal;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface ParamProposalRepository extends JpaRepository<ParamProposal, Long> {

  @Query("SELECT pp.registeredTx.id as transaction, "
      + " pp.epochNo AS epoch  "
      + "FROM ParamProposal pp "
      + "GROUP BY pp.epochNo,  pp.registeredTx.id "
      + "ORDER BY pp.registeredTx.id DESC, pp.epochNo DESC")
  List<ParamChange> findHistoryTransactionForEachEpoch();

  @Query("SELECT DISTINCT pp "
      + "FROM ParamProposal pp "
      + "WHERE pp.registeredTx.id >= :txId "
      + "ORDER BY pp.registeredTx.id DESC")
  List<ParamProposal> getAllDistinctProtocolParam(@Param("txId") Long txId);

  List<ParamProposal> getParamProposalByRegisteredTxId(Long id);
}
