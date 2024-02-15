package org.cardanofoundation.explorer.api.repository.explorer;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.common.entity.explorer.VerifiedScript;

public interface VerifiedScriptRepository extends JpaRepository<VerifiedScript, Long> {

  Boolean existsVerifiedScriptByHash(@Param("hash") String hash);

  Optional<VerifiedScript> findByHash(@Param("hash") String hash);
}
