package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.math.BigInteger;
import java.security.Timestamp;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.common.entity.compositeKey.AddressBalanceId;
import org.cardanofoundation.explorer.common.entity.ledgersync.AddressBalance;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeAddress;

public interface AddressBalanceRepository extends JpaRepository<AddressBalance, AddressBalanceId> {
  @Query(value = """
    SELECT SUM(ab.quantity) AS amount
    FROM AddressBalance ab
    WHERE ab.address in (select add.address from Address add where add.stakeAddress = :stakeAddress)
    and not exists(select 1 from AddressBalance ab1 where ab1.address = ab.address and ab1.slot > ab.slot)
    and ab.blockTime <= :time and ab.unit = 'lovelace'
""")
  Optional<BigInteger> getBalanceByStakeAddressAndTime(
      @Param("stakeAddress") StakeAddress stakeAddress, @Param("time") Long time);
}
