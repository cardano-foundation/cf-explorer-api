package org.cardanofoundation.explorer.api.model.response.pool;

import java.math.BigInteger;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@Getter
@Setter
@SuperBuilder
@NoArgsConstructor
@AllArgsConstructor
public class PoolRangeValuesResponse {
  BigInteger minPoolSize;
  BigInteger maxPoolSize;

  BigInteger minPledge;
  BigInteger maxPledge;

  Double minSaturation;
  Double maxSaturation;

  Integer minLifetimeBlock;
  Integer maxLifetimeBlock;

  Double minVotingPower;
  Double maxVotingPower;

  Double minGovParticipationRate;
  Double maxGovParticipationRate;
}
