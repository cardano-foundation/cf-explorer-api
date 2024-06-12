package org.cardanofoundation.explorer.api.repository.explorer;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import org.cardanofoundation.explorer.common.entity.enumeration.TokenAuthType;
import org.cardanofoundation.explorer.common.entity.explorer.TokenAuth;

public interface TokenAuthRepository extends JpaRepository<TokenAuth, Long> {

  @Query(
      "select ta from TokenAuth ta where ta.token = :token and ta.tokenAuthType = :tokenAuthType")
  Optional<TokenAuth> findByToken(
      @Param("token") String token, @Param("tokenAuthType") TokenAuthType tokenAuthType);
}
