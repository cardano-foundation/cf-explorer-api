package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.cardanofoundation.explorer.api.projection.AddressQuantityProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.AddressBalance;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;

public interface AddressBalanceRepository extends JpaRepository<AddressBalance, Long> {

//    @Query(value =
//            """
//            SELECT a.address as address, a.quantity as quantity FROM AddressBalance a
//            LEFT OUTER JOIN AddressBalance b
//            ON a.address = b.address AND a.slot < b.slot WHERE b.address IS NULL AND a.unit = 'lovelace'
//            ORDER BY a.quantity DESC LIMIT :count
//            """)
    @Query(value = """
            SELECT a.address as address, a.quantity as quantity FROM AddressBalance a
            WHERE a.slot = (SELECT max(slot) FROM AddressBalance b WHERE b.address = a.address)
            AND unit = 'lovelace'
            ORDER BY quantity DESC LIMIT :count
            """)
    List<AddressQuantityProjection> findTopAddresses(@Param("count") int count);

}
