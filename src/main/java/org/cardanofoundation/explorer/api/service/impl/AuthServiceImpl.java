package org.cardanofoundation.explorer.api.service.impl;

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
    try {
      return tokenAuthRepository
          .findByToken(token, tokenAuthType)
          .map(TokenAuth::getBlackList)
          .orElse(Boolean.FALSE);
    } catch (Exception e) {
      return Boolean.FALSE;
    }
  }
}
