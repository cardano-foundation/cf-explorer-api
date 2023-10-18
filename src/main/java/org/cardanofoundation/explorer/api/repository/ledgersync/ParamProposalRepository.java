package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.sql.Timestamp;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;


import org.cardanofoundation.explorer.api.projection.LatestParamHistory;
import org.cardanofoundation.explorer.api.projection.ParamHistory;
import org.cardanofoundation.explorer.consumercommon.entity.ParamProposal;

@Repository
public interface ParamProposalRepository extends JpaRepository<ParamProposal, Long> {

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
      + "pp.costModelId AS costModel, pp.registeredTxId AS tx, pp.epochNo AS epochNo "
      + "FROM ParamProposal  pp "
      + "GROUP BY pp.minFeeA, pp.minFeeB, pp.maxBlockSize, pp.maxTxSize,pp.maxBhSize,pp.keyDeposit, "
      + "pp.poolDeposit, pp.maxEpoch, pp.optimalPoolCount, "
      + "pp.influence, pp.monetaryExpandRate, "
      + "pp.treasuryGrowthRate, pp.decentralisation, pp.entropy, pp.protocolMajor, "
      + "pp.protocolMinor, pp.minUtxoValue, pp.minPoolCost, pp.priceMem, pp.priceStep, pp.maxTxExMem, "
      + "pp.maxTxExSteps, pp.maxBlockExMem, pp.maxBlockExSteps, pp.maxValSize, pp.collateralPercent, "
      + "pp.maxCollateralInputs, pp.coinsPerUtxoSize, pp.costModel.id, pp.registeredTx.id, pp.epochNo")
  List<ParamHistory> findProtocolsChange();

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
      + "pp.costModelId AS costModel, pp.registeredTxId AS tx, e.no  AS epochNo "
      + "FROM ParamProposal  pp "
      + "RIGHT JOIN Epoch e ON e.no = pp.epochNo "
      + "WHERE e.startTime <= :epochTime "
      + "GROUP BY pp.minFeeA, pp.minFeeB, pp.maxBlockSize, pp.maxTxSize,pp.maxBhSize,pp.keyDeposit, "
      + "pp.poolDeposit, pp.maxEpoch, pp.optimalPoolCount, "
      + "pp.influence, pp.monetaryExpandRate, "
      + "pp.treasuryGrowthRate, pp.decentralisation, pp.entropy, pp.protocolMajor, "
      + "pp.protocolMinor, pp.minUtxoValue, pp.minPoolCost, pp.priceMem, pp.priceStep, pp.maxTxExMem, "
      + "pp.maxTxExSteps, pp.maxBlockExMem, pp.maxBlockExSteps, pp.maxValSize, pp.collateralPercent, "
      + "pp.maxCollateralInputs, pp.coinsPerUtxoSize, pp.costModel.id, pp.registeredTx.id, e.no")
  List<ParamHistory> findProtocolsChange(@Param("epochTime") Timestamp epochTime);

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
      + "pp.costModelId AS costModel, pp.registeredTxId as tx, pp.epochNo AS epochNo  "
      + "FROM ParamProposal  pp "
      + "WHERE pp.epochNo = :epochNo "
      + "GROUP BY pp.minFeeA, pp.minFeeB, pp.maxBlockSize, pp.maxTxSize,pp.maxBhSize,pp.keyDeposit, "
      + "pp.poolDeposit, pp.maxEpoch, pp.optimalPoolCount, "
      + "pp.influence, pp.monetaryExpandRate, "
      + "pp.treasuryGrowthRate, pp.decentralisation, pp.entropy, pp.protocolMajor, "
      + "pp.protocolMinor, pp.minUtxoValue, pp.minPoolCost, pp.priceMem, pp.priceStep, pp.maxTxExMem, "
      + "pp.maxTxExSteps, pp.maxBlockExMem, pp.maxBlockExSteps, pp.maxValSize, pp.collateralPercent, "
      + "pp.maxCollateralInputs, pp.coinsPerUtxoSize, pp.costModelId, pp.registeredTxId, pp.epochNo")
  List<ParamHistory> findEpochProtocolsChange(@Param("epochNo") Integer epochNo);

  @Query(""" 
      SELECT MAX(ep.epochNo) 
      FROM ParamProposal pp
      JOIN EpochParam ep ON ep.epochNo = pp.epochNo + 1
      """)
  Integer findMaxEpochChange();

  @Query("SELECT CASE WHEN pp.minFeeA IS NOT NULL THEN pp.minFeeA ELSE ep.minFeeA END  AS minFeeA, "
      + " CASE WHEN pp.minFeeB IS NOT NULL THEN pp.minFeeB ELSE ep.minFeeB END  AS minFeeB,  "
      + " CASE WHEN pp.maxBlockSize IS NOT NULL THEN pp.maxBlockSize ELSE ep.maxBlockSize END  AS maxBlockSize,  "
      + " CASE WHEN pp.maxTxSize IS NOT NULL THEN pp.maxTxSize ELSE ep.maxTxSize END  AS maxTxSize,  "
      + " CASE WHEN pp.maxBhSize IS NOT NULL THEN pp.maxBhSize ELSE ep.maxBhSize END  AS maxBhSize,  "
      + " CASE WHEN pp.keyDeposit IS NOT NULL THEN pp.keyDeposit ELSE ep.keyDeposit END  AS keyDeposit,  "
      + " CASE WHEN pp.poolDeposit IS NOT NULL THEN pp.poolDeposit ELSE ep.poolDeposit END  AS poolDeposit,  "
      + " CASE WHEN pp.maxEpoch IS NOT NULL THEN pp.maxEpoch ELSE ep.maxEpoch END  AS maxEpoch,  "
      + " CASE WHEN pp.optimalPoolCount IS NOT NULL THEN pp.optimalPoolCount ELSE ep.optimalPoolCount END  AS optimalPoolCount,  "
      + " CASE WHEN pp.influence IS NOT NULL THEN pp.influence ELSE ep.influence END  AS influence,  "
      + " CASE WHEN pp.monetaryExpandRate IS NOT NULL THEN pp.monetaryExpandRate ELSE ep.monetaryExpandRate END  AS monetaryExpandRate,  "
      + " CASE WHEN pp.treasuryGrowthRate IS NOT NULL THEN pp.treasuryGrowthRate ELSE ep.treasuryGrowthRate END  AS treasuryGrowthRate,  "
      + " CASE WHEN pp.decentralisation IS NOT NULL THEN pp.decentralisation ELSE ep.decentralisation END  AS decentralisation,  "
      + " CASE WHEN pp.entropy IS NOT NULL THEN pp.entropy ELSE ep.extraEntropy END  AS entropy,  "
      + " CASE WHEN pp.protocolMajor IS NOT NULL THEN pp.protocolMajor ELSE ep.protocolMajor END  AS protocolMajor,  "
      + " CASE WHEN pp.protocolMinor IS NOT NULL THEN pp.protocolMinor ELSE ep.protocolMinor END  AS protocolMinor,  "
      + " CASE WHEN pp.minUtxoValue IS NOT NULL THEN pp.minUtxoValue ELSE ep.minUtxoValue END  AS minUtxoValue,  "
      + " CASE WHEN pp.minPoolCost IS NOT NULL THEN pp.minPoolCost ELSE ep.minPoolCost END  AS minPoolCost,  "
      + " CASE WHEN pp.priceMem IS NOT NULL THEN pp.priceMem ELSE ep.priceMem END  AS priceMem,  "
      + " CASE WHEN pp.priceStep IS NOT NULL THEN pp.priceStep ELSE ep.priceStep END  AS priceStep,  "
      + " CASE WHEN pp.maxTxExMem IS NOT NULL THEN pp.maxTxExMem ELSE ep.maxTxExMem END  AS maxTxExMem,  "
      + " CASE WHEN pp.maxTxExSteps IS NOT NULL THEN pp.maxTxExSteps ELSE ep.maxTxExSteps END  AS maxTxExSteps,  "
      + " CASE WHEN pp.maxBlockExMem IS NOT NULL THEN pp.maxBlockExMem ELSE ep.maxBlockExMem END  AS maxBlockExMem,  "
      + " CASE WHEN pp.maxBlockExSteps IS NOT NULL THEN pp.maxBlockExSteps ELSE ep.maxBlockExSteps END  AS maxBlockExSteps,  "
      + " CASE WHEN pp.maxValSize IS NOT NULL THEN pp.maxValSize ELSE ep.maxValSize END  AS maxValSize,  "
      + " CASE WHEN pp.collateralPercent IS NOT NULL THEN pp.collateralPercent ELSE ep.collateralPercent END  AS collateralPercent,  "
      + " CASE WHEN pp.maxCollateralInputs IS NOT NULL THEN pp.maxCollateralInputs ELSE ep.maxCollateralInputs END  AS maxCollateralInputs,  "
      + " CASE WHEN pp.coinsPerUtxoSize IS NOT NULL THEN pp.coinsPerUtxoSize ELSE ep.coinsPerUtxoSize END  AS coinsPerUtxoSize,  "
      + " CASE WHEN pp.costModelId IS NOT NULL THEN pp.costModelId "
      + "      ELSE ep.costModel.id "
      + " END AS costModel,  "// check Change
      + " CASE WHEN pp.minFeeA IS NOT NULL THEN TRUE ELSE FALSE END  AS minFeeAProposal, "
      + " CASE WHEN pp.minFeeB IS NOT NULL THEN TRUE ELSE FALSE END  AS minFeeBProposal,  "
      + " CASE WHEN pp.maxBlockSize IS NOT NULL THEN TRUE ELSE FALSE END  AS maxBlockSizeProposal,  "
      + " CASE WHEN pp.maxTxSize IS NOT NULL THEN TRUE ELSE FALSE END  AS maxTxSizeProposal,  "
      + " CASE WHEN pp.maxBhSize IS NOT NULL THEN TRUE ELSE FALSE END  AS maxBhSizeProposal,  "
      + " CASE WHEN pp.keyDeposit IS NOT NULL THEN TRUE ELSE FALSE END  AS keyDepositProposal,  "
      + " CASE WHEN pp.poolDeposit IS NOT NULL THEN TRUE ELSE FALSE END  AS poolDepositProposal,  "
      + " CASE WHEN pp.maxEpoch IS NOT NULL THEN TRUE ELSE FALSE END  AS maxEpochProposal,  "
      + " CASE WHEN pp.optimalPoolCount IS NOT NULL THEN TRUE ELSE FALSE END  AS optimalPoolCountProposal,  "
      + " CASE WHEN pp.influence IS NOT NULL THEN TRUE ELSE FALSE END  AS influenceProposal,  "
      + " CASE WHEN pp.monetaryExpandRate IS NOT NULL THEN TRUE ELSE FALSE END  AS monetaryExpandRateProposal,  "
      + " CASE WHEN pp.treasuryGrowthRate IS NOT NULL THEN TRUE ELSE FALSE END  AS treasuryGrowthRateProposal,  "
      + " CASE WHEN pp.decentralisation IS NOT NULL THEN TRUE ELSE FALSE END  AS decentralisationProposal,  "
      + " CASE WHEN pp.entropy IS NOT NULL THEN TRUE ELSE FALSE END  AS entropyProposal,  "
      + " CASE WHEN pp.protocolMajor IS NOT NULL THEN TRUE ELSE FALSE END  AS protocolMajorProposal,  "
      + " CASE WHEN pp.protocolMinor IS NOT NULL THEN TRUE ELSE FALSE END  AS protocolMinorProposal,  "
      + " CASE WHEN pp.minUtxoValue IS NOT NULL THEN TRUE ELSE FALSE END  AS minUtxoValueProposal,  "
      + " CASE WHEN pp.minPoolCost IS NOT NULL THEN TRUE ELSE FALSE END  AS minPoolCostProposal,  "
      + " CASE WHEN pp.priceMem IS NOT NULL THEN TRUE ELSE FALSE END  AS priceMemProposal,  "
      + " CASE WHEN pp.priceStep IS NOT NULL THEN TRUE ELSE FALSE END  AS priceStepProposal,  "
      + " CASE WHEN pp.maxTxExMem IS NOT NULL THEN TRUE ELSE FALSE END  AS maxTxExMemProposal,  "
      + " CASE WHEN pp.maxTxExSteps IS NOT NULL THEN TRUE ELSE FALSE END  AS maxTxExStepsProposal,  "
      + " CASE WHEN pp.maxBlockExMem IS NOT NULL THEN TRUE ELSE FALSE END  AS maxBlockExMemProposal,  "
      + " CASE WHEN pp.maxBlockExSteps IS NOT NULL THEN TRUE ELSE FALSE END  AS maxBlockExStepsProposal,  "
      + " CASE WHEN pp.maxValSize IS NOT NULL THEN TRUE ELSE FALSE END  AS maxValSizeProposal,  "
      + " CASE WHEN pp.collateralPercent IS NOT NULL THEN TRUE ELSE FALSE END  AS collateralPercentProposal,  "
      + " CASE WHEN pp.maxCollateralInputs IS NOT NULL THEN TRUE ELSE FALSE END  AS maxCollateralInputsProposal,  "
      + " CASE WHEN pp.coinsPerUtxoSize IS NOT NULL THEN TRUE ELSE FALSE END  AS coinsPerUtxoSizeProposal,  "
      + " CASE WHEN pp.costModelId IS NOT NULL THEN TRUE ELSE FALSE END AS costModelProposal, "
      + " b.time  AS blockTime,  "
      + " e.startTime AS epochTime, "
      + " ep.epochNo AS epochNo, "
      + " tx.hash AS hash "
      + " FROM EpochParam ep  "
      + " LEFT JOIN ParamProposal  pp ON pp.epochNo + 1 = ep.epochNo "
      + " LEFT JOIN Tx tx ON tx.id = pp.registeredTxId "
      + " LEFT JOIN Block b ON b.id =  tx.blockId "
      + " INNER JOIN Epoch e ON e.no = ep.epochNo "
      + " WHERE ep.epochNo <= :epochNo "
      + " ORDER BY ep.epochNo DESC ")
  List<LatestParamHistory> findProtocolsChange(@Param("epochNo") Integer epochNo);

}
