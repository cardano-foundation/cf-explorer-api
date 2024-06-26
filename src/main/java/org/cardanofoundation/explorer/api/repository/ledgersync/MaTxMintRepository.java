package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.math.BigInteger;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.api.projection.MintProjection;
import org.cardanofoundation.explorer.common.entity.ledgersync.MaTxMint;
import org.cardanofoundation.explorer.common.entity.ledgersync.MaTxMint_;
import org.cardanofoundation.explorer.common.entity.ledgersync.MultiAsset;

public interface MaTxMintRepository extends JpaRepository<MaTxMint, Long> {

  @Query(
      "SELECT ma.name as name, ma.policy as policy, mtm.quantity as assetQuantity,"
          + " ma.fingerprint as fingerprint, am.url as url, am.ticker as ticker,"
          + " am.decimals as decimals, am.logo as logo, am.description as description"
          + " FROM MaTxMint mtm "
          + " JOIN MultiAsset ma ON ma.id = mtm.ident.id"
          + " LEFT JOIN AssetMetadata am ON am.fingerprint = ma.fingerprint"
          + " WHERE mtm.tx.id=:txId")
  List<MintProjection> findByTxId(@Param("txId") Long txId);

  @Query(
      "SELECT maTxMint"
          + " FROM MaTxMint maTxMint "
          + " WHERE maTxMint.ident.fingerprint = :tokenId ")
  @EntityGraph(attributePaths = {MaTxMint_.TX, "tx.block"})
  Page<MaTxMint> findByIdent(@Param("tokenId") String tokenId, Pageable pageable);

  @Query(
      value =
          "SELECT tm.json FROM Tx tx"
              + " JOIN MaTxMint mtm ON mtm.tx = tx"
              + " JOIN TxMetadata tm ON tm.tx = tx"
              + " JOIN MultiAsset ma ON ma = mtm.ident"
              + " WHERE ma.fingerprint = :fingerprint AND tm.key = :label"
              + " ORDER BY mtm.id DESC LIMIT 1")
  String getTxMetadataToken(
      @Param("fingerprint") String fingerprint, @Param("label") BigInteger label);

  @Query(
      value = "SELECT mtm.tx_id from ma_tx_mint mtm where mtm.ident = :multiAssetId LIMIT 1",
      nativeQuery = true)
  Long findFirstTxMintByMultiAssetId(@Param("multiAssetId") Long multiAssetId);

  @Query(
      value =
          "SELECT TRUE FROM MaTxMint mtm "
              + "WHERE mtm.ident IN :multiAssets "
              + "AND mtm.tx.id != :txId")
  Boolean existsMoreOneMintTx(
      @Param("multiAssets") List<MultiAsset> multiAssets, @Param("txId") Long txId);

  @Query(
      value =
          "SELECT CASE WHEN count(mtm.id) >= 1 THEN TRUE ELSE FALSE END AS FLAG "
              + "FROM MaTxMint mtm "
              + "JOIN MultiAsset ma ON ma = mtm.ident "
              + "WHERE ma.fingerprint = :fingerprint AND mtm.quantity = 1")
  Boolean mintNumber(@Param("fingerprint") String fingerprint);
}
