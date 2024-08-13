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
public class GovActionMetaDataAuthor {
  @JsonProperty("name")
  String name;

  @JsonProperty("witness")
  Witness witness;
}
