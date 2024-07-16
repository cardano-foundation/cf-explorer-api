package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.math.BigInteger;
import java.util.List;
import java.util.Optional;

import org.cardanofoundation.explorer.api.projection.MinMaxProjection;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.common.entity.compositeKey.StakeAddressBalanceId;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeAddressBalance;

public interface StakeAddressBalanceRepository
    extends JpaRepository<StakeAddressBalance, StakeAddressBalanceId> {

  @Query(
      value =
          """
            SELECT a.quantity from StakeAddressBalance a
            WHERE a.slot = (SELECT MAX(b.slot) FROM StakeAddressBalance b WHERE b.address = a.address)
            AND a.address = :address
            """)
  Optional<BigInteger> findStakeQuantityByAddress(@Param("address") String stakeAddress);

  @Query(
      value =
          """
                    SELECT COALESCE(SUM(a.quantity), 0)  from StakeAddressBalance a
                    WHERE a.slot = (SELECT MAX(b.slot) FROM StakeAddressBalance b WHERE b.address = a.address)
                    AND a.address IN :addresses
                    """)
  BigInteger getBalanceByView(@Param("addresses") List<String> stakeAddress);


  @Query(
          value =
                  """
                            SELECT address, MIN(quantity) as minVal, MAX(quantity) as maxVal
                            FROM StakeAddressBalance
                            WHERE address = :address
                            AND slot > :fromDate
                            AND slot <= :toDate
                            GROUP BY address
                            """)
  MinMaxProjection findMinMaxBalanceByAddressInSlotRange(
          @Param("address") String address,
          @Param("fromDate") Long fromDate,
          @Param("toDate") Long toDate);

}
