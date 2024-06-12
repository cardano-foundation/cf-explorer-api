package org.cardanofoundation.explorer.api.service.impl;

import java.util.Optional;

import lombok.RequiredArgsConstructor;

import org.springframework.stereotype.Service;

import org.cardanofoundation.explorer.api.repository.explorer.TokenAuthRepository;
import org.cardanofoundation.explorer.api.service.AuthService;
import org.cardanofoundation.explorer.common.entity.enumeration.TokenAuthType;
import org.cardanofoundation.explorer.common.entity.explorer.TokenAuth;

@Service
@RequiredArgsConstructor
public class AuthServiceImpl implements AuthService {

  private final TokenAuthRepository tokenAuthRepository;

  @Override
  public Boolean isBlacklistToken(String token, TokenAuthType tokenAuthType) {
    Optional<TokenAuth> tokenAuth = tokenAuthRepository.findByToken(token, tokenAuthType);
    if (tokenAuth.isEmpty()) {
      return Boolean.FALSE;
    }
    return tokenAuth.get().getBlackList();
  }
}
