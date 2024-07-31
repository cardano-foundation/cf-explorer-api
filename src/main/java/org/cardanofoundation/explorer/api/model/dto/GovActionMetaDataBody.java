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
public class GovActionMetaDataBody {
  @JsonProperty("title")
  String title;

  @JsonProperty("abstract")
  String abstractContent;

  @JsonProperty("motivation")
  String motivation;

  @JsonProperty("rationale")
  String rationale;

  @JsonProperty("references")
  Object references;
}
