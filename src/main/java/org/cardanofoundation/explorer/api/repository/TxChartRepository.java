package org.cardanofoundation.explorer.api.repository;

import java.math.BigInteger;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.TxGraphProjection;
import org.cardanofoundation.explorer.consumercommon.entity.TxChart;

public interface TxChartRepository extends JpaRepository<TxChart, Long> {

  @Query("SELECT SUM(tx.txSimple) AS simpleTransactions,"
      + " SUM(tx.txWithMetadataWithoutSc) AS smartContract ,"
      + "SUM(tx.txWithSc) AS metadata, "
      + "tx.hour AS time FROM TxChart tx "
      + "WHERE tx.hour in :hours "
      + "GROUP BY tx.hour "
      + "ORDER BY tx.hour DESC")
  List<TxGraphProjection> getTransactionGraphByHour(@Param("hours") List<BigInteger> hours);

  @Query("SELECT SUM(tx.txSimple) AS simpleTransactions,"
      + " SUM(tx.txWithMetadataWithoutSc) AS smartContract ,"
      + "SUM(tx.txWithSc) AS metadata, "
      + "tx.day AS time FROM TxChart tx "
      + "WHERE tx.day in :day "
      + "GROUP BY tx.day "
      + "ORDER BY tx.day DESC")
  List<TxGraphProjection> getTransactionGraphByDay(@Param("day") List<BigInteger> hours);


  @Query("SELECT SUM(tx.txSimple) AS simpleTransactions,"
      + " SUM(tx.txWithMetadataWithoutSc) AS smartContract ,"
      + "SUM(tx.txWithSc) AS metadata, "
      + "tx.day AS time FROM TxChart tx "
      + "WHERE tx.day >= :day "
      + "GROUP BY tx.day "
      + "ORDER BY tx.day DESC")
  List<TxGraphProjection> getTransactionGraphDayGreaterThan(@Param("day") BigInteger date);
}
