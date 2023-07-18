package org.cardanofoundation.explorer.api.model.response.token;

import lombok.*;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class TokenMetadataResponse {
  private String url;
  private String ticker;
  private Integer decimals;
  private String logo;
  private String description;
}
