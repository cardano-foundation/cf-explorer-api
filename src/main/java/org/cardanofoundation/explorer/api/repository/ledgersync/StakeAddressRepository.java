package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.math.BigInteger;
import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.apache.kafka.common.quota.ClientQuotaAlteration.Op;
import org.cardanofoundation.explorer.api.projection.StakeAddressProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeAddress;

public interface StakeAddressRepository extends JpaRepository<StakeAddress, Long> {

  @Query(value = """
    SELECT sa from StakeAddress sa
    WHERE sa.view = :view
""")
  Optional<StakeAddress> findByView(@Param("view") String view);

  @Query(
      value =
          """
      SELECT sa.id as id, sa.view as stakeAddress, sab.quantity as totalStake
                    FROM StakeAddress sa
                    JOIN StakeAddressBalance sab ON sab.address = sa.view
                      AND NOT EXISTS (SELECT 1 FROM StakeAddressBalance sab1 WHERE sab1.address = sa.view AND sab1.slot > sab.slot)
                    WHERE EXISTS (SELECT d FROM Delegation d WHERE d.stakeAddressId = sa.id)
                    AND (SELECT max(sr.txId) FROM StakeRegistration sr WHERE sr.stakeAddressId = sa.id) >
                    (SELECT COALESCE(max(sd.txId), 0) FROM StakeDeregistration sd WHERE sd.stakeAddressId = sa.id)
                    ORDER BY totalStake DESC
      """)
  List<StakeAddressProjection> findStakeAddressOrderByBalance(Pageable pageable);

  @Query(
      value =
          "SELECT ph.view FROM StakeAddress sa "
              + "JOIN PoolOwner po ON sa.id  = po.stakeAddress.id "
              + "JOIN PoolUpdate pu ON po.poolUpdate.id  = pu.id "
              + "JOIN PoolHash ph ON pu.poolHash.id = ph.id "
              + "WHERE sa.view = :stakeKey "
              + "GROUP BY ph.view ")
  Page<String> getPoolViewByStakeKey(@Param("stakeKey") String stakeKey, Pageable pageable);

  @Query(value = "SELECT sa.view FROM StakeAddress sa WHERE sa.id IN :addressIds")
  List<String> getViewByAddressId(@Param("addressIds") Set<Long> addressIds);

  List<StakeAddress> findByIdIn(Collection<Long> ids);

  @Query("SELECT stake.view" + " FROM StakeAddress stake" + " WHERE stake.scriptHash = :scriptHash")
  List<String> getStakeAssociatedAddress(@Param("scriptHash") String scriptHash);

  @Query(value = """
    SELECT COALESCE(SUM(sab.quantity), 0) FROM StakeAddressBalance sab
    WHERE sab.address IN :views
    AND NOT EXISTS(SELECT 1 FROM StakeAddressBalance sab1 WHERE sab1.address = sab.address AND sab1.slot > sab.slot)
""")
  BigInteger getBalanceByView(@Param("views") List<String> views);

  @Query(value = "SELECT sa.view FROM StakeAddress sa WHERE sa.scriptHash = :scriptHash")
  List<String> getAssociatedAddress(@Param("scriptHash") String scriptHash);
}
