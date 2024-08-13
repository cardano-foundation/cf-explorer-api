package org.cardanofoundation.explorer.api.model.response.governanceAction;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AuthorResponse {

  private String name;

  private String witnessAlgorithm;

  private String publicKey;

  private String signature;
}
