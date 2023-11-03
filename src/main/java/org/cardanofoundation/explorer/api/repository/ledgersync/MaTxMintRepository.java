package org.cardanofoundation.explorer.api.repository.ledgersync;

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

  @Query(value = "SELECT tm.json from Tx tx"
      + " JOIN MaTxMint mtm ON mtm.tx = tx"
      + " JOIN TxMetadata tm ON tm.tx = tx"
      + " JOIN MultiAsset ma ON ma = mtm.ident"
      + " WHERE ma.supply = 1"
      + " AND ma.fingerprint = :fingerprint"
      + " ORDER BY mtm.id DESC LIMIT 1")
  String getTxMetadataNFTToken(@Param("fingerprint") String fingerprint);

  @Query(value = "SELECT COALESCE(COUNT(DISTINCT mtm.tx), 0)"
      + " FROM MaTxMint mtm"
      + " INNER JOIN MultiAsset ma ON ma = mtm.ident"
      + " WHERE ma.policy = :policy")
  Long countByPolicy(@Param("policy") String policy);
}
