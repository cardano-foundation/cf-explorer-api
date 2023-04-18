package com.cardano.explorer.repository;

import com.cardano.explorer.projection.ParamChange;
import com.cardano.explorer.projection.ParamHistory;
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

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.decentralisation as decentralisation, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryDecentralisation(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.minFeeA as minFeeA, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryMinFeeA(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.minFeeB as minFeeB, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryMinFeeB(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.maxBlockSize as maxBlockSize, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryMaxBlockSize(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.maxTxSize as maxTxSize, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryMaxTxSize(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.maxBhSize as maxBhSize, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryMaxBhSize(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.keyDeposit AS keyDeposit, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryKeyDeposit(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.poolDeposit AS poolDeposit, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryPoolDeposit(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.maxEpoch AS maxEpoch, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryMaxEpoch(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.optimalPoolCount AS optimalPoolCount, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryOptimalPoolCount(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.minUtxoValue AS minUtxoValue, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryMinUtxoValue(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.minPoolCost AS minPoolCost, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryMinPoolCost(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.maxTxExMem AS maxTxExMem, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryMaxTxExMem(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.maxTxExSteps AS maxTxExSteps, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryMaxTxExSteps(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.maxBlockExMem AS maxBlockExMem, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryMaxBlockExMem(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.maxBlockExSteps AS maxBlockExSteps, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryMaxBlockExSteps(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.maxValSize AS maxValSize, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryMaxValSize(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.coinsPerUtxoSize AS coinsPerUtxoSize, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryCoinsPerUtxoSize(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.influence AS influence, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryInfluence(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.monetaryExpandRate AS monetaryExpandRate, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryMonetaryExpandRate(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.treasuryGrowthRate AS treasuryGrowthRate, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryTreasuryGrowthRate(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.priceMem AS priceMem, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryPriceMem(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.priceStep AS priceStep, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryPriceStep(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.protocolMajor AS protocolMajor, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryProtocolMajor(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.protocolMinor AS protocolMinor, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryProtocolMinor(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.collateralPercent AS collateralPercent, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryCollateralPercent(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.maxCollateralInputs AS maxCollateralInputs, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryMaxCollateralInputs(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.entropy AS entropy, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryEntropy(@Param("txIds") List<Long> txIds);

  @Query(
      "SELECT DISTINCT pp.epochNo AS epoch, pp.costModel AS costModel, b.time AS time, tx.hash AS hash "
          + "FROM ParamProposal pp "
          + "JOIN Tx tx ON tx.id = pp.registeredTx.id AND "
          + "pp.registeredTx.id IN :txIds "
          + "JOIN Block b ON b.id =  tx.blockId "
  )
  List<ParamHistory> getHistoryCostModel(@Param("txIds") List<Long> txIds);


}
