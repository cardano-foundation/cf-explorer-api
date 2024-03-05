package org.cardanofoundation.explorer.api.model.response.contract;

import lombok.*;

import com.fasterxml.jackson.annotation.JsonInclude;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
@JsonInclude
public class ContractScript {
  private Boolean isVerified;
  private String data;
}
