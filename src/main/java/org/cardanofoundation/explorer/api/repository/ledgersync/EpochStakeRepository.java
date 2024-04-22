package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;
import java.util.Set;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import org.cardanofoundation.explorer.api.projection.StakeAddressProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.EpochStake;

@Repository
public interface EpochStakeRepository extends JpaRepository<EpochStake, Long> {

  @Query(
      value =
          "SELECT es.addr.id AS address, es.amount AS totalStake FROM EpochStake es "
              + "WHERE es.addr.id IN :stakeAddressIds "
              + "AND es.epochNo = :epochNo")
  List<StakeAddressProjection> totalStakeByAddressAndPool(
      @Param("stakeAddressIds") Set<Long> stakeAddressIds, @Param("epochNo") Integer epochNo);
}
