package com.cardano.explorer.repository;

import com.cardano.explorer.projection.TokenProjection;
import com.sotatek.cardano.common.entity.MaTxMint;
import com.sotatek.cardano.common.entity.MaTxMint_;
import com.sotatek.cardano.common.entity.MultiAsset;
import com.sotatek.cardano.common.entity.Tx;
import java.sql.Timestamp;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface MaTxMintRepository extends JpaRepository<MaTxMint, Long> {

  @EntityGraph(attributePaths = {MaTxMint_.IDENT})
  List<MaTxMint> findByTx(Tx tx);

  @Query("SELECT mint.ident.id as ident, min(block.time) as createdOn"
      + " FROM MaTxMint mint "
      + " INNER JOIN Tx tx ON tx = mint.tx"
      + " INNER JOIN Block block ON tx.blockId = block.id"
      + " WHERE mint.ident IN :multiAssets"
      + " GROUP BY mint.ident.id")
  List<TokenProjection> findCreatedOnByIdentIn(List<MultiAsset> multiAssets);

  @Query("SELECT min(block.time) as createdOn"
      + " FROM MaTxMint mint "
      + " INNER JOIN Tx tx ON tx = mint.tx"
      + " INNER JOIN Block block ON tx.blockId = block.id"
      + " WHERE mint.ident = :multiAsset")
  Timestamp findCreatedOnByIdent(MultiAsset multiAsset);

  @Query("SELECT maTxMint"
      + " FROM MaTxMint maTxMint "
      + " WHERE maTxMint.ident.fingerprint = :tokenId ")
  @EntityGraph(attributePaths = {MaTxMint_.TX, "tx.block"})
  Page<MaTxMint> findByIdent(String tokenId, Pageable pageable);

}
