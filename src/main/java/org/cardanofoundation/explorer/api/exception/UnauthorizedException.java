package org.cardanofoundation.explorer.api.exception;

import org.cardanofoundation.explorer.common.exception.AbstractTokenException;
import org.cardanofoundation.explorer.common.exception.CommonErrorCode;

public class UnauthorizedException extends AbstractTokenException {

  public UnauthorizedException() {
    super(CommonErrorCode.UNAUTHORIZED_TOKEN);
  }
}
