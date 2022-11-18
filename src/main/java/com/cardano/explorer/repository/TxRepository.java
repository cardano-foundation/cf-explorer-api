package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.Block;
import com.sotatek.cardano.common.entity.Tx;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

public interface TxRepository extends JpaRepository<Tx, Long>, JpaSpecificationExecutor<Tx> {

  List<Tx> findByBlockIn(List<Block> blocks);

  @EntityGraph(attributePaths = {"block"})
  Optional<Tx> findByHash(String hash);
}
