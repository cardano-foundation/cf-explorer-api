package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.StakeAddressBalanceProjection;
import org.cardanofoundation.explorer.common.entity.compositeKey.StakeAddressBalanceId;
import org.cardanofoundation.explorer.common.entity.ledgersync.LatestStakeAddressBalance;

public interface LatestStakeAddressBalanceRepository
    extends JpaRepository<LatestStakeAddressBalance, StakeAddressBalanceId> {

  @Query("SELECT s FROM LatestStakeAddressBalance s WHERE s.address = :stakeAddress")
  Optional<LatestStakeAddressBalance> findByStakeAddress(
      @Param("stakeAddress") String stakeAddress);

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
}
