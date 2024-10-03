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

import org.cardanofoundation.explorer.common.entity.ledgersync.StakeAddress;

public interface StakeAddressRepository extends JpaRepository<StakeAddress, Long> {

  Optional<StakeAddress> findByView(@Param("aLong") String aLong);

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
