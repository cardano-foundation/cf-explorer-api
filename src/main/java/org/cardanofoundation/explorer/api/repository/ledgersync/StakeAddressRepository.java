package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.StakeAddressProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeAddress;

public interface StakeAddressRepository extends JpaRepository<StakeAddress, Long> {

  Optional<StakeAddress> findByView(@Param("aLong") String aLong);

  @Query(
      value =
          "SELECT sa.id as id, sa.view as stakeAddress"
              + " FROM StakeAddress sa"
              + " WHERE sa.view IN :stakeAddresses "
              + " AND EXISTS (SELECT d FROM Delegation d WHERE d.address = sa)"
              + " AND (SELECT max(sr.txId) FROM StakeRegistration sr WHERE sr.addr = sa) >"
              + " (SELECT COALESCE(max(sd.txId), 0) FROM StakeDeregistration sd WHERE sd.addr = sa)")
  List<StakeAddressProjection> findStakeAddressOrderByBalance(
      @Param("stakeAddresses") Collection<String> stakeAddresses);

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

  @Query(value = "SELECT sa.view FROM StakeAddress sa WHERE sa.scriptHash = :scriptHash")
  List<String> getAssociatedAddress(@Param("scriptHash") String scriptHash);
}
