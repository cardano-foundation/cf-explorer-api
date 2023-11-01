package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.sql.Timestamp;
import java.util.Collection;

import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
import org.cardanofoundation.explorer.api.projection.PolicyProjection;
import org.cardanofoundation.explorer.api.projection.TokenProjection;
import org.cardanofoundation.explorer.consumercommon.entity.MultiAsset;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;

import java.util.List;
import java.util.Optional;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface MultiAssetRepository extends JpaRepository<MultiAsset, Long> {

  @Query(value = "SELECT ma.id as id, ma.policy as policy, ma.name as name, ma.name_view as nameView, ma.tx_count as txCount,"
      + " ma.fingerprint as fingerprint, ma.supply as supply, ma.total_volume as totalVolume, ma.time as time,"
      + " LENGTH(ma.name_view) as nameViewLength"
      + " FROM multi_asset ma "
      + " WHERE ma.fingerprint = :query OR LOWER(ma.name_view) LIKE CONCAT('%', :query, '%')",
      countQuery = "SELECT COUNT(*) FROM (SELECT 1 FROM multi_asset ma "
          + "WHERE ma.fingerprint = :query OR LOWER(ma.name_view) LIKE CONCAT('%', :query, '%') limit 1000) as A",
      nativeQuery = true)
  Page<TokenProjection> findAll(@Param("query") String query, Pageable pageable);

  @Query("select multiAsset from MultiAsset multiAsset")
  List<MultiAsset> findMultiAssets(Pageable pageable);

  Optional<MultiAsset> findByFingerprint(@Param("fingerprint") String fingerprint);

  Integer countByPolicy(@Param("policy") String policy);

  Page<MultiAsset> findAllByPolicy(@Param("policy") String policy, Pageable pageable);

  List<MultiAsset> findAllByIdIn(@Param("ids") Collection<Long> ids);

  @Query("SELECT b.time FROM Tx tx JOIN Block b ON b.id = tx.blockId "
      + "WHERE tx.id = (SELECT max(adt.txId) FROM AddressToken adt WHERE adt.multiAsset = :multiAsset)")
  Timestamp getLastActivityTimeOfToken(@Param("multiAsset") MultiAsset multiAsset);

  @Query("SELECT ma FROM MultiAsset ma WHERE lower(ma.nameView) LIKE CONCAT('%', :query, '%') ")
  List<MultiAsset> findByNameViewLike(@Param("query") String query, Pageable pageable);

  @Query("SELECT ma.fingerprint as fingerprint, ma.name as tokenName, mtm.quantity as quantity FROM MultiAsset ma "
      + "JOIN MaTxMint mtm on (mtm.ident = ma and mtm.tx = :tx and ma.policy = :policy)")
  List<AddressTokenProjection> getMintingAssets(@Param("tx") Tx tx, @Param("policy") String policy);

  @Query("SELECT COALESCE(count(multiAsset.id), 0) as numberOfTokens, multiAsset.policy as policy"
      + " FROM MultiAsset multiAsset"
      + " WHERE multiAsset.policy IN :policyList"
      + " GROUP BY multiAsset.policy")
  List<PolicyProjection> countMultiAssetByPolicyIn(@Param("policyList") List<String> policyList);

  @Query("SELECT COALESCE(COUNT(DISTINCT(atb.addressId, atb.multiAssetId)), 0) as numberOfAssetHolders, ma.policy as policy "
      + " FROM MultiAsset ma "
      + " INNER JOIN AddressTokenBalance atb ON atb.multiAsset = ma"
      + " WHERE ma.policy IN :policyList "
      + " AND atb.balance > 0 "
      + " GROUP BY ma.policy")
  List<PolicyProjection> countAssetHoldersByPolicyIn(@Param("policyList") List<String> policyList);
}
