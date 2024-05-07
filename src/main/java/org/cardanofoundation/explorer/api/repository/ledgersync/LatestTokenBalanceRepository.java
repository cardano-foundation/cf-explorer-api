package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;

import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.common.entity.compositeKey.AddressBalanceId;
import org.cardanofoundation.explorer.common.entity.ledgersync.LatestTokenBalance;

public interface LatestTokenBalanceRepository extends
    JpaRepository<LatestTokenBalance, AddressBalanceId> {

  @Query(
      value = """
  
  SELECT ltb.address as address, ltb.quantity as quantity, ma.nameView as tokenName, ma.fingerprint as fingerprint
            FROM LatestTokenBalance ltb
            inner JOIN MultiAsset ma ON ma.unit = ltb.unit
            WHERE ma.policy = :policy
  order by ltb.quantity desc
""")
  List<AddressTokenProjection> findAddressAndBalanceByPolicy(
      @Param("policy") String policy, Pageable pageable);
}
