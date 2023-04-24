package org.cardanofoundation.explorer.api.repository;

import com.sotatek.cardano.common.entity.Script;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ScriptRepository extends JpaRepository<Script, Long> {

  Optional<Script> findByHash(String hash);

}
