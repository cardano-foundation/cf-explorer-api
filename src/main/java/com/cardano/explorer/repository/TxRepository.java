package com.cardano.explorer.repository;

import com.cardano.explorer.entity.Block;
import com.cardano.explorer.entity.Tx;
import java.util.List;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TxRepository extends JpaRepository<Tx, Long> {

  List<Tx> findByBlockIn(List<Block> blocks);

  Optional<Tx> findByHash(byte[] hash);
}
