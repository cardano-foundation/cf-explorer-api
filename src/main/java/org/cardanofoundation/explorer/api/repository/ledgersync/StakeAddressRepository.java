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

import org.cardanofoundation.explorer.api.projection.SmartContractProjection;
import org.cardanofoundation.explorer.api.projection.StakeAddressProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.StakeAddress;

public interface StakeAddressRepository extends JpaRepository<StakeAddress, Long> {

  Optional<StakeAddress> findByView(@Param("aLong") String aLong);

  @Query(
      value =
          "SELECT sa.id as id, sa.view as stakeAddress, sab.quantity as totalStake"
              + " FROM StakeAddress sa"
              + " JOIN StakeAddressBalance sab ON sa.view = sab.address"
              + " AND sab.quantity IS NOT NULL"
              + " ORDER BY totalStake DESC")
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

  @Query(
      "SELECT stake.view as address, stake.scriptHash as scriptHash"
          + " FROM StakeAddress stake"
          + " WHERE stake.scriptHash IN :scriptHashList")
  List<SmartContractProjection> findStakeAssociatedAddressByHashIn(
      @Param("scriptHashList") List<String> scriptHashList);

  @Query("SELECT stake.view" + " FROM StakeAddress stake" + " WHERE stake.scriptHash = :scriptHash")
  List<String> getStakeAssociatedAddress(@Param("scriptHash") String scriptHash);

  @Query(
      value =
          "SELECT COALESCE(SUM(sab.quantity), 0) FROM StakeAddress sa JOIN StakeAddressBalance sab ON sab.address = sa.view WHERE sa.view IN :views")
  BigInteger getBalanceByView(@Param("views") List<String> views);

  @Query(value = "SELECT sa.view FROM StakeAddress sa WHERE sa.scriptHash = :scriptHash")
  List<String> getAssociatedAddress(@Param("scriptHash") String scriptHash);

  @Query(value = "SELECT sa FROM StakeAddress sa WHERE sa.view IN :views")
  List<StakeAddress> findByViewIn(@Param("views") Collection<String> views);
}
