package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.api.projection.StakeAddressProjection;
import org.cardanofoundation.explorer.consumercommon.entity.StakeAddress;

import java.util.Collection;
import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface StakeAddressRepository extends JpaRepository<StakeAddress, Long> {

  Optional<StakeAddress> findByView(@Param("aLong") String aLong);

  @Query(value = "SELECT sa.id as id, sa.view as stakeAddress, sum(addr.balance) as totalStake"
      + " FROM StakeAddress sa"
      + " LEFT JOIN Address addr ON addr.stakeAddress = sa"
      + " WHERE EXISTS (SELECT d FROM Delegation d WHERE d.address = sa)"
      + " GROUP BY sa.id"
      + " HAVING sum(addr.balance) IS NOT NULL"
      + " ORDER BY totalStake DESC")
  List<StakeAddressProjection> findStakeAddressOrderByBalance(Pageable pageable);

  @Query(value = "SELECT sa.view as stakeAddress, sum(addr.balance) as totalStake"
      + " FROM StakeAddress sa"
      + " LEFT JOIN Address addr ON addr.stakeAddress = sa"
      + " WHERE sa.id in (:stakeKeyIds)"
      + " GROUP BY sa.id"
      + " HAVING sum(addr.balance) IS NOT NULL"
      + " ORDER BY totalStake DESC")
  List<StakeAddressProjection> findStakeAddressOrderByBalance(@Param("stakeKeyIds") Collection<Long> stakeKeyIds,
                                                              Pageable pageable);

  @Query(value = "SELECT ph.view FROM StakeAddress sa "
      + "JOIN PoolOwner po ON sa.id  = po.stakeAddress.id "
      + "JOIN PoolUpdate pu ON po.poolUpdate.id  = pu.id "
      + "JOIN PoolHash ph ON pu.poolHash.id = ph.id "
      + "WHERE sa.view = :stakeKey "
      + "GROUP BY ph.view ")
  Page<String> getPoolViewByStakeKey(@Param("stakeKey") String stakeKey, Pageable pageable);
}
