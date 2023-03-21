package com.cardano.explorer.model.response.token;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import java.math.BigDecimal;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@JsonInclude(Include.NON_NULL)
public class TokenAddressResponse {
  private String address;
  private String name;
  private String displayName;
  private String fingerprint;
  private BigDecimal quantity;
}
