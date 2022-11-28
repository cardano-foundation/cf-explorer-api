package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.MaTxMint;
import com.sotatek.cardano.common.entity.Tx;
import java.util.List;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;

public interface MaTxMintRepository extends JpaRepository<MaTxMint, Long> {

  @EntityGraph(attributePaths = {"ident"})
  List<MaTxMint> findByTx(Tx tx);
}
