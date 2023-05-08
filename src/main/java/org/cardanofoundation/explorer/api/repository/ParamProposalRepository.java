package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.projection.ParamChange;
import org.cardanofoundation.explorer.api.projection.ParamHistory;
import org.cardanofoundation.explorer.consumercommon.entity.ParamProposal;
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

  @Query("SELECT pp "
      + "FROM ParamProposal  pp "
      + "WHERE pp.registeredTx.id = :id "
      + "ORDER BY pp.id DESC"
  )
  List<ParamProposal> getParamProposalByRegisteredTxId(@Param("id") Long id);

  @Query("SELECT pp "
      + "FROM ParamProposal  pp "
      + "WHERE pp.registeredTx.id < :id "
      + "ORDER BY pp.id DESC"
  )
  List<ParamProposal> getParamProposalBySmallerThanRegisteredTxId(@Param("id") Long id);

  @Query("SELECT pp.minFeeA AS minFeeA , pp.minFeeB AS minFeeB, pp.maxBlockSize AS maxBlockSize ,"
      + "pp.maxTxSize AS maxTxSize,pp.maxBhSize AS maxBhSize,pp.keyDeposit AS keyDeposit, "
      + "pp.poolDeposit AS poolDeposit, pp.maxEpoch AS maxEpoch, pp.optimalPoolCount AS optimalPoolCount, "
      + "pp.influence AS influence, pp.monetaryExpandRate AS monetaryExpandRate, "
      + "pp.treasuryGrowthRate AS treasuryGrowthRate, pp.decentralisation AS decentralisation,"
      + "pp.entropy AS entropy, pp.protocolMajor AS protocolMajor, pp.protocolMinor AS protocolMinor, "
      + "pp.minUtxoValue AS minUtxoValue, pp.minPoolCost AS minPoolCost, pp.priceMem AS priceMem,"
      + " pp.priceStep AS priceStep, pp.maxTxExMem AS maxTxExMem, pp.maxTxExSteps AS maxTxExSteps, "
      + "pp.maxBlockExMem AS maxBlockExMem, pp.maxBlockExSteps AS maxBlockExSteps, "
      + "pp.maxValSize AS maxValSize, pp.collateralPercent AS collateralPercent, "
      + "pp.maxCollateralInputs AS maxCollateralInputs, pp.coinsPerUtxoSize AS coinsPerUtxoSize,"
      + "pp.costModel.id AS costModel, pp.registeredTx.id as tx , MAX(pp.id) AS id "
      + "FROM ParamProposal  pp "
      + "WHERE pp.epochNo < :epochNo "
      + "GROUP BY pp.minFeeA, pp.minFeeB, pp.maxBlockSize, pp.maxTxSize,pp.maxBhSize,pp.keyDeposit, "
      + "pp.poolDeposit, pp.maxEpoch, pp.optimalPoolCount, "
      + "pp.influence, pp.monetaryExpandRate, "
      + "pp.treasuryGrowthRate, pp.decentralisation, pp.entropy, pp.protocolMajor, "
      + "pp.protocolMinor, pp.minUtxoValue, pp.minPoolCost, pp.priceMem, pp.priceStep, pp.maxTxExMem, "
      + "pp.maxTxExSteps, pp.maxBlockExMem, pp.maxBlockExSteps, pp.maxValSize, pp.collateralPercent, "
      + "pp.maxCollateralInputs, pp.coinsPerUtxoSize, pp.costModel.id, pp.registeredTx.id")
  List<ParamHistory> findProtocolsChange(@Param("epochNo") Integer epochNo);


  @Query("SELECT pp "
      + "FROM ParamProposal pp "
      + "WHERE pp.registeredTx.id < :txId AND pp.epochNo >= :epochNo "
      + "ORDER BY pp.id DESC")
  List<ParamProposal> getParamProposalSmallerRegisteredTxId(@Param("txId") Long txId, Integer epochNo);
}
