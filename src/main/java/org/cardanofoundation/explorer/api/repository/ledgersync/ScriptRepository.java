package org.cardanofoundation.explorer.api.repository.ledgersync;

import java.util.List;
import java.util.Optional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.consumercommon.entity.Script;
import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptType;

public interface ScriptRepository extends JpaRepository<Script, Long> {

  Optional<Script> findByHash(@Param("hash") String hash);

  Page<Script> findAllByTypeIn(@Param("typeList") List<ScriptType> typeList, Pageable pageable);

  List<Script> findAllByHashIn(@Param("hashList") List<String> hashList);
}
