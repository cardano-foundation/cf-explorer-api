package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.cardanofoundation.explorer.api.projection.SmartContractProjection;
import org.cardanofoundation.explorer.consumercommon.entity.Script;

import java.util.List;
import java.util.Optional;

import org.cardanofoundation.explorer.consumercommon.enumeration.ScriptType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

public interface ScriptRepository extends JpaRepository<Script, Long> {

  Optional<Script> findByHash(@Param("hash") String hash);

  Page<Script> findAllByTypeIn(@Param("typeList") List<ScriptType> typeList, Pageable pageable);

}
