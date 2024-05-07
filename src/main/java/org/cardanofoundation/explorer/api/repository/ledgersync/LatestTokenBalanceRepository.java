package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.springframework.data.domain.Page;
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

  @Query(value =
      """ 
          SELECT ma.fingerprint as fingerprint,
          ma.policy as policy,
          ma.name as tokenName,
          ltb.quantity as quantity,
          am.url as url, am.decimals as decimals, am.ticker as ticker,
          am.logo as logo, am.description as description, am.subject as subject
          FROM LatestTokenBalance ltb
          INNER JOIN MultiAsset ma ON ma.unit = ltb.unit
          LEFT JOIN AssetMetadata am ON ma.fingerprint = ma.fingerprint
          WHERE ltb.address = :address
          AND ltb.quantity > 0
          """)
  Page<AddressTokenProjection> findTokenAndBalanceByAddress(
      @Param("address") String address, Pageable pageable);

  @Query(value =
      """ 
          SELECT ma.fingerprint as fingerprint,
          ma.policy as policy,
          ma.name as tokenName,
          ltb.quantity as quantity,
          am.url as url, am.decimals as decimals, am.ticker as ticker,
          am.logo as logo, am.description as description, am.subject as subject
          FROM LatestTokenBalance ltb
          INNER JOIN MultiAsset ma ON ma.unit = ltb.unit
          LEFT JOIN AssetMetadata am ON ma.fingerprint = ma.fingerprint
          WHERE ltb.address = :address
          AND (lower(ma.nameView) LIKE CONCAT('%', :searchValue, '%') OR ma.fingerprint = :searchValue)
          AND ltb.quantity > 0
          """)
  Page<AddressTokenProjection> findTokenAndBalanceByAddressAndNameView(
      @Param("address") String address,
      @Param("searchValue") String searchValue,
      Pageable pageable);

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
