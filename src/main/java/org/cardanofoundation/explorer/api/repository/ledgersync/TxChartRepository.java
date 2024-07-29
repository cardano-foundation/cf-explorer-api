package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.math.BigInteger;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.TxGraphProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.TxChart;

public interface TxChartRepository extends JpaRepository<TxChart, Long> {

  @Query(
      "SELECT SUM(tx.txSimple) AS simpleTransactions,"
          + " SUM(tx.txWithMetadataWithoutSc) AS metadata ,"
          + "SUM(tx.txWithSc) AS smartContract, "
          + "tx.day AS time FROM TxChart tx "
          + "WHERE tx.day in :day "
          + "GROUP BY tx.day ")
  List<TxGraphProjection> getTransactionGraphByDay(@Param("day") List<BigInteger> day);

  @Query(
      "SELECT SUM(tx.txSimple) AS simpleTransactions,"
          + " SUM(tx.txWithMetadataWithoutSc) AS  metadata,"
          + "SUM(tx.txWithSc) AS smartContract, "
          + "tx.day AS time FROM TxChart tx "
          + "WHERE tx.day >= :day "
          + "GROUP BY tx.day ")
  List<TxGraphProjection> getTransactionGraphDayGreaterThan(@Param("day") BigInteger day);

  @Query(
      "SELECT SUM(tx.txSimple) AS simpleTransactions,"
          + " SUM(tx.txWithMetadataWithoutSc) AS  metadata,"
          + "SUM(tx.txWithSc) AS smartContract, "
          + "tx.month AS time FROM TxChart tx "
          + "WHERE :month is null or (tx.month >= :month) "
          + "GROUP BY tx.month ")
  List<TxGraphProjection> getTransactionGraphMonthGreaterThan(@Param("month") BigInteger month);
}
