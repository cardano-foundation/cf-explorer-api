package org.cardanofoundation.explorer.api.model.response.committee;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.FieldDefaults;
import lombok.experimental.SuperBuilder;

import org.cardanofoundation.explorer.api.common.enumeration.CommitteeStatus;

@Getter
@Setter
@SuperBuilder
@AllArgsConstructor
@NoArgsConstructor
@FieldDefaults(level = AccessLevel.PRIVATE)
public class CommitteeMemberResponse {

  String publicKey;
  CommitteeStatus status;
  Integer expiredEpoch;
  Integer activeEpoch;
}
