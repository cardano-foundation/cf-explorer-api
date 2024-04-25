package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;
import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.common.entity.ledgersync.Script;

public interface ScriptRepository extends JpaRepository<Script, Long> {

  Optional<Script> findByHash(@Param("hash") String hash);

  List<Script> findAllByHashIn(@Param("hashList") List<String> hashList);
}
