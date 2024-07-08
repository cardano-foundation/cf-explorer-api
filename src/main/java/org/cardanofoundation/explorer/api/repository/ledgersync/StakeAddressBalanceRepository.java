package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.cardanofoundation.explorer.common.entity.compositeKey.StakeAddressBalanceId;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeAddressBalance;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.math.BigInteger;
import java.util.Optional;

public interface StakeAddressBalanceRepository extends JpaRepository<StakeAddressBalance, StakeAddressBalanceId> {

    @Query(value = """
            SELECT a.quantity from StakeAddressBalance a 
            WHERE a.slot = (SELECT MAX(b.slot) FROM StakeAddressBalance b WHERE b.address = a.address)
            AND a.address = :address
            """)
    Optional<BigInteger> findStakeQuantityByAddress(@Param("address") String stakeAddress);
}
