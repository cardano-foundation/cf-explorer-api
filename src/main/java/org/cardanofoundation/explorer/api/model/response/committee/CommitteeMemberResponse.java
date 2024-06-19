package org.cardanofoundation.explorer.api.model.response.committee;

import java.util.Date;

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
  String scriptHash;
  CommitteeStatus status;
  Integer expiredEpoch;
  Integer activeEpoch;

  Date registeredAt;
  Integer termDuration;
  Float votingParticipation;
  Date resignedAt;
}
