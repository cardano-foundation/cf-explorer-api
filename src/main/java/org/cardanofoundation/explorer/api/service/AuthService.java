package org.cardanofoundation.explorer.api.service;

import org.cardanofoundation.explorer.common.entity.enumeration.TokenAuthType;

public interface AuthService {
  Boolean isBlacklistToken(String token, TokenAuthType tokenAuthType);
}
