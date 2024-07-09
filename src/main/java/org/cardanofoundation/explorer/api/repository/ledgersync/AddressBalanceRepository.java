package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.cardanofoundation.explorer.api.projection.MinMaxProjection;
import org.cardanofoundation.explorer.common.entity.compositeKey.AddressBalanceId;
import org.cardanofoundation.explorer.common.entity.ledgersync.AddressBalance;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface AddressBalanceRepository extends JpaRepository<AddressBalance, AddressBalanceId> {

    @Query(value = """
                    SELECT address, MIN(quantity) as minVal, MAX(quantity) as maxVal
                    FROM AddressBalance
                    WHERE address = :address
                    AND blockTime > :fromDate
                    AND blockTime <= :toDate
                    AND unit = 'lovelace'
                    GROUP BY address
                    """)
    MinMaxProjection findMinMaxBalanceByAddress(@Param("address") String address,
                                                @Param("fromDate") Long fromDate,
                                                @Param("toDate") Long toDate);
}
