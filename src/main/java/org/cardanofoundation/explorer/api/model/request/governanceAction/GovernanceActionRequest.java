package org.cardanofoundation.explorer.api.model.request.governanceAction;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

import org.cardanofoundation.explorer.common.entity.enumeration.VoterType;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
public class GovernanceActionRequest {
  String txHash;

  Integer index;

  VoterType voterType;
}
