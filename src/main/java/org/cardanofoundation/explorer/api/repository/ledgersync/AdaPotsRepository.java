package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.math.BigInteger;
import java.util.Optional;

import org.cardanofoundation.explorer.consumercommon.entity.AdaPots;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

@Repository
public interface AdaPotsRepository extends JpaRepository<AdaPots, Long> {

  Boolean existsByEpochNo(@Param("epochNo") Integer epochNo);

  @Query(value = "SELECT ap.reserves FROM AdaPots ap WHERE ap.epochNo = :epochNo")
  BigInteger getReservesByEpochNo(@Param("epochNo") Integer epochNo);

}
