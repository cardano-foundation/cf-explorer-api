package com.cardano.explorer.repository;

import com.sotatek.cardano.common.entity.Script;
import java.util.Optional;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

public interface ScriptRepository extends JpaRepository<Script, Long> {

  @Query("SELECT script.json FROM Script script WHERE script.hash = :hash")
  Optional<String> findJsonByHash(String hash);

}
