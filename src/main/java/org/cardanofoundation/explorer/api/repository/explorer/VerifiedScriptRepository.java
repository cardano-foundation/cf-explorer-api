package org.cardanofoundation.explorer.api.repository.explorer;

import org.cardanofoundation.explorer.consumercommon.explorer.entity.VerifiedScript;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.query.Param;

import java.util.Optional;

public interface VerifiedScriptRepository extends JpaRepository<VerifiedScript, Long> {

  Boolean existsVerifiedScriptByHash(@Param("hash") String hash);

  Optional<VerifiedScript> findByHash(@Param("hash") String hash);
}
