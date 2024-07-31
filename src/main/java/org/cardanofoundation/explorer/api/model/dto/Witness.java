package org.cardanofoundation.explorer.api.model.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import com.fasterxml.jackson.annotation.JsonProperty;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Witness {
  @JsonProperty("witnessAlgorithm")
  String witnessAlgorithm;

  @JsonProperty("publicKey")
  String publicKey;

  @JsonProperty("signature")
  String signature;
}
