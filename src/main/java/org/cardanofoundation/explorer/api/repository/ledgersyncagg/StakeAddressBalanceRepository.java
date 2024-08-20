package org.cardanofoundation.explorer.api.repository.ledgersyncagg;

import java.math.BigInteger;
import java.util.Collection;
import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.StakeAddressBalanceProjection;
import org.cardanofoundation.explorer.common.entity.compositeKey.StakeAddressBalanceId;
import org.cardanofoundation.explorer.common.entity.ledgersyncsagg.StakeAddressBalance;

public interface StakeAddressBalanceRepository
    extends JpaRepository<StakeAddressBalance, StakeAddressBalanceId> {

  @Query(
      value =
          """
          SELECT sab.address as address,
                 sab.quantity as balance
          FROM stake_address_balance_view sab
          WHERE sab.address = :stakeAddress
        """,
      nativeQuery = true)
  StakeAddressBalanceProjection findLatestBalanceByStakeAddress(
      @Param("stakeAddress") String stakeAddress);

  @Query(
      value =
          """
                  SELECT coalesce(sum(sab.quantity), 0)
                  FROM stake_address_view sav
                  CROSS JOIN LATERAL ( SELECT tmp.address,
                                        tmp.quantity
                                 FROM stake_address_balance tmp
                                 WHERE tmp.address = sav.stake_address
                                 ORDER BY tmp.slot DESC
                                 LIMIT 1) sab
                  WHERE sav.stake_address IN :stakeAddresses
              """,
      nativeQuery = true)
  BigInteger sumBalanceByStakeAddressIn(@Param("stakeAddresses") Collection<String> stakeAddresses);

  @Query(
      value =
          """
                  SELECT sab.address as address,
                         sab.quantity as balance
                  FROM stake_address_balance_view sab
                  WHERE sab.address in :stakeAddress
                """,
      nativeQuery = true)
  List<StakeAddressBalanceProjection> findLatestBalanceByStakeAddressIn(
      @Param("stakeAddress") Collection<String> stakeAddress);
}
