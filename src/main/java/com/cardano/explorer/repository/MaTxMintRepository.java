package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.MaTxMint;
import com.sotatek.cardano.common.entity.MaTxMint_;
import com.sotatek.cardano.common.entity.Tx;
import java.util.List;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface MaTxMintRepository extends JpaRepository<MaTxMint, Long> {

  @EntityGraph(attributePaths = {MaTxMint_.IDENT})
  List<MaTxMint> findByTx(Tx tx);

  @Query("SELECT maTxMint"
      + " FROM MaTxMint maTxMint "
      + " WHERE maTxMint.ident.fingerprint = :tokenId ")
  @EntityGraph(attributePaths = {MaTxMint_.TX, "tx.block"})
  Page<MaTxMint> findByIdent(String tokenId, Pageable pageable);

}
