package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.sql.Timestamp;
import java.util.Collection;

import org.cardanofoundation.explorer.api.projection.AddressTokenProjection;
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
      + " LENGTH(ma.name_view) as nameViewLength, "
      + " am.url as url, am.ticker as ticker, am.decimals as decimals, "
      + " am.logo as logo, am.description as description, am.subject as subject"
      + " FROM multi_asset ma"
      + " LEFT JOIN asset_metadata am ON am.fingerprint = ma.fingerprint"
      + " WHERE ma.fingerprint = :query OR LOWER(ma.name_view) LIKE CONCAT('%', :query, '%')",
      countQuery = "SELECT COUNT(*) FROM (SELECT 1 FROM multi_asset ma "
          + "WHERE ma.fingerprint = :query OR LOWER(ma.name_view) LIKE CONCAT('%', :query, '%') limit 1000) as A",
      nativeQuery = true)
  Page<TokenProjection> findAll(@Param("query") String query, Pageable pageable);

  @Query(value = "SELECT ma.id as id, ma.policy as policy, ma.name as name, ma.nameView as nameView, ma.txCount as txCount,"
      + " ma.fingerprint as fingerprint, ma.supply as supply, ma.totalVolume as totalVolume, ma.time as time,"
      + " LENGTH(ma.nameView) as nameViewLength, "
      + " am.url as url, am.ticker as ticker, am.decimals as decimals, "
      + " am.logo as logo, am.description as description, am.subject as subject"
      + " FROM MultiAsset ma"
      + " LEFT JOIN AssetMetadata am ON am.fingerprint = ma.fingerprint")
  List<TokenProjection> findMultiAssets(Pageable pageable);

  Optional<MultiAsset> findByFingerprint(@Param("fingerprint") String fingerprint);

  Integer countByPolicy(@Param("policy") String policy);

  Page<MultiAsset> findAllByPolicy(@Param("policy") String policy, Pageable pageable);

  @Query(value = "SELECT ma.id as id, ma.policy as policy, ma.name as name, ma.nameView as nameView, ma.txCount as txCount,"
      + " ma.fingerprint as fingerprint, ma.supply as supply, ma.totalVolume as totalVolume, ma.time as time,"
      + " am.subject as subject, am.url as url, am.ticker as ticker, "
      + " am.decimals as decimals, am.logo as logo, am.description as description"
      + " FROM MultiAsset ma"
      + " LEFT JOIN AssetMetadata am ON am.fingerprint = ma.fingerprint"
      + " WHERE ma.policy = :scriptHash")
  List<TokenProjection> findTokenInfoByScriptHash(@Param("scriptHash") String scriptHash,Pageable pageable);

  List<MultiAsset> findAllByIdIn(@Param("ids") Collection<Long> ids);

  @Query("SELECT b.time FROM Tx tx JOIN Block b ON b.id = tx.blockId "
      + "WHERE tx.id = (SELECT max(adt.txId) FROM AddressToken adt WHERE adt.multiAsset = :multiAsset)")
  Timestamp getLastActivityTimeOfToken(@Param("multiAsset") MultiAsset multiAsset);

  @Query("SELECT ma FROM MultiAsset ma WHERE lower(ma.nameView) LIKE CONCAT('%', :query, '%') ")
  List<MultiAsset> findByNameViewLike(@Param("query") String query, Pageable pageable);

  @Query("SELECT ma.fingerprint as fingerprint, ma.name as tokenName, mtm.quantity as quantity FROM MultiAsset ma "
      + "JOIN MaTxMint mtm on (mtm.ident = ma and mtm.tx = :tx and ma.policy = :policy)")
  List<AddressTokenProjection> getMintingAssets(@Param("tx") Tx tx, @Param("policy") String policy);

  @Query("SELECT COALESCE(count(multiAsset.id), 0)"
      + " FROM MultiAsset multiAsset"
      + " WHERE multiAsset.policy = :policy")
  Long countMultiAssetByPolicy(@Param("policy") String policy);

  @Query("SELECT COALESCE(COUNT(DISTINCT(atb.addressId, atb.multiAssetId)), 0) "
      + " FROM MultiAsset ma "
      + " INNER JOIN AddressTokenBalance atb ON atb.multiAsset = ma"
      + " WHERE ma.policy = :policy "
      + " AND atb.balance > 0 ")
  Long countAssetHoldersByPolicy(@Param("policy") String policy);

  @Query(value = "WITH firstResult AS ("
      + " SELECT topMultiAsset.*"
      + " FROM script s"
      + " CROSS JOIN LATERAL"
      + " (SELECT ma.name as name, ma.name_view as name_view, ma.policy as policy, ma.fingerprint as fingerprint"
      + " FROM multi_asset ma "
      + " WHERE ma.policy = s.hash ORDER BY ma.tx_count DESC LIMIT 5)"
      + " AS topMultiAsset WHERE s.hash IN :scriptHashes)"
      + " SELECT firstResult.policy as policy, firstResult.name as name, firstResult.name_view as nameView, firstResult.fingerprint as fingerprint,"
      + " am.url as url, am.ticker as ticker, am.decimals as decimals, "
      + " am.logo as logo, am.description as description, am.subject as subject"
      + " FROM firstResult"
      + " LEFT JOIN asset_metadata am ON am.fingerprint = firstResult.fingerprint",nativeQuery = true)
  List<TokenProjection> findTopMultiAssetByScriptHashIn(@Param("scriptHashes") List<String> scriptHashes);
}
