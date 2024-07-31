package org.cardanofoundation.explorer.api.model.dto;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import com.fasterxml.jackson.annotation.JsonProperty;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class GovActionMetaData {
  @JsonProperty("authors")
  List<GovActionMetaDataAuthor> authors;
}
