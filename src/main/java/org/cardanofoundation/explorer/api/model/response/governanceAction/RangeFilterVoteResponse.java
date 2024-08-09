package org.cardanofoundation.explorer.api.model.response.governanceAction;

import java.math.BigInteger;

import lombok.*;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RangeFilterVoteResponse {
  BigInteger minActiveStake;
  BigInteger maxActiveStake;
}
