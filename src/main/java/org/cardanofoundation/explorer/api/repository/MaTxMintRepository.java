package org.cardanofoundation.explorer.api.repository;

import org.cardanofoundation.explorer.consumercommon.entity.MaTxMint;
import org.cardanofoundation.explorer.consumercommon.entity.MaTxMint_;
import org.cardanofoundation.explorer.consumercommon.entity.Tx;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface MaTxMintRepository extends JpaRepository<MaTxMint, Long> {

  @EntityGraph(attributePaths = {MaTxMint_.IDENT})
  List<MaTxMint> findByTx(@Param("tx") Tx tx);

  @Query("SELECT maTxMint"
      + " FROM MaTxMint maTxMint "
      + " WHERE maTxMint.ident.fingerprint = :tokenId ")
  @EntityGraph(attributePaths = {MaTxMint_.TX, "tx.block"})
  Page<MaTxMint> findByIdent(@Param("tokenId") String tokenId, Pageable pageable);

  @Query(value = "SELECT tm.json FROM tx_metadata tm "
      + "LEFT JOIN ma_tx_mint mtm on mtm.tx_id = tm.tx_id "
      + "WHERE tm.key = 721 "
      + "AND tm.ts_json @@ to_tsquery('simple', :tsQuery)"
      + "ORDER BY mtm.id DESC LIMIT 1", nativeQuery = true)
  String getTxMetadataNFTToken(@Param("tsQuery") String tsQuery);
}