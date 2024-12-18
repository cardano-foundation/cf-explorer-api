package org.cardanofoundation.explorer.api.repository.ledgersyncagg;

import java.util.List;

import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.common.entity.compositeKey.AddressBalanceId;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.LatestTokenBalance;

public interface LatestTokenBalanceRepository
    extends JpaRepository<LatestTokenBalance, AddressBalanceId> {

  @Query(
      value =
          """
      SELECT ltb.address as address, ltb.quantity as quantity, ltb.unit as unit
      FROM LatestTokenBalance ltb
      WHERE ltb.policy = :policy
      AND ltb.quantity > 0
      """)
  List<AddressTokenProjection> findAddressAndBalanceByPolicy(
      @Param("policy") String policy, Pageable pageable);

  @Query(
      value =
          """
          SELECT COALESCE(COUNT(latestTokenBalance), 0) as numberOfHolders
          FROM LatestTokenBalance latestTokenBalance
          WHERE latestTokenBalance.policy = :policy
    """)
  Long countAssetHoldersByPolicy(@Param("policy") String policy);

  List<LatestTokenBalance> findAllByAddress(String address);
}
