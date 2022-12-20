package com.cardano.explorer.model.response.token;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class TokenMetadataResponse {
  private String url;
  private String ticker;
  private Integer decimals;
  private String logo;
  private String description;
}
