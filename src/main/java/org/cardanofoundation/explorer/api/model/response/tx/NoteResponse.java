package org.cardanofoundation.explorer.api.model.response.tx;

import lombok.Builder;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@Builder
public class NoteResponse {
  private String note;
}
