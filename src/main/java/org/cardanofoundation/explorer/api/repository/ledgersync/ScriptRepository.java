package org.cardanofoundation.explorer.api.repository.ledgersync;

import org.cardanofoundation.explorer.consumercommon.entity.Script;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.query.Param;

public interface ScriptRepository extends JpaRepository<Script, Long> {

  Optional<Script> findByHash(@Param("hash") String hash);

}
