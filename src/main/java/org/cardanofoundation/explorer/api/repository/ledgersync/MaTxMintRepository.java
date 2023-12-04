package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.math.BigInteger;
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

  @Query(value = "SELECT tm.json FROM Tx tx"
      + " JOIN MaTxMint mtm ON mtm.tx = tx"
      + " JOIN TxMetadata tm ON tm.tx = tx"
      + " JOIN MultiAsset ma ON ma = mtm.ident"
      + " WHERE ma.fingerprint = :fingerprint AND tm.key = :label"
      + " ORDER BY mtm.id DESC LIMIT 1")
  String getTxMetadataToken(@Param("fingerprint") String fingerprint, @Param("label") BigInteger label);
  
  @Query(value = "SELECT COALESCE(COUNT(DISTINCT mtm.tx), 0)"
      + " FROM MaTxMint mtm"
      + " INNER JOIN MultiAsset ma ON ma = mtm.ident"
      + " WHERE ma.policy = :policy")
  Long countByPolicy(@Param("policy") String policy);

  @Query(value = "SELECT CASE WHEN count(mtm.id) >= 1 THEN TRUE ELSE FALSE END AS FLAG "
      + "FROM MaTxMint mtm "
      + "JOIN MultiAsset ma ON ma = mtm.ident "
      + "WHERE ma.fingerprint = :fingerprint AND mtm.quantity = 1")
  Boolean mintNumber(@Param("fingerprint") String fingerprint);
}