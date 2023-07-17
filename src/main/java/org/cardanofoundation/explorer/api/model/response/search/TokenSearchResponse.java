package org.cardanofoundation.explorer.api.model.response.search;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class TokenSearchResponse {
  private String name;
  private String fingerprint;
}
