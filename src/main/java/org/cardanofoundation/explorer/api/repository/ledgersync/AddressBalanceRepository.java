package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.MinMaxProjection;
import org.cardanofoundation.explorer.common.entity.compositeKey.AddressBalanceId;
import org.cardanofoundation.explorer.common.entity.ledgersync.AddressBalance;

public interface AddressBalanceRepository extends JpaRepository<AddressBalance, AddressBalanceId> {

  @Query(
      value =
          """
                    SELECT address, MIN(quantity) as minVal, MAX(quantity) as maxVal
                    FROM AddressBalance
                    WHERE address = :address
                    AND slot > :fromDate
                    AND slot <= :toDate
                    AND unit = 'lovelace'
                    GROUP BY address
                    """)
  MinMaxProjection findMinMaxBalanceByAddressInSlotRange(
      @Param("address") String address,
      @Param("fromDate") Long fromDate,
      @Param("toDate") Long toDate);
}
