package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.math.BigInteger;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.common.entity.ledgersync.StakeAddressBalance;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeAddressBalanceId;

@Repository
public interface StakeAddressBalanceRepository
    extends JpaRepository<StakeAddressBalance, StakeAddressBalanceId> {

  @Query(
      value =
          "SELECT COALESCE(SUM(sa.quantity), 0) FROM StakeAddressBalance sa WHERE sa.address = :address")
  Optional<BigInteger> getQuantityByAddress(@Param("address") String address);
}
