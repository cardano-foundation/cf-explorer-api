package org.cardanofoundation.explorer.api.model.request.pool;

import java.math.BigInteger;

import lombok.AllArgsConstructor;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.experimental.SuperBuilder;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@SuperBuilder
@EqualsAndHashCode
public class PoolListFilter {
  String query;

  BigInteger maxPoolSize;

  BigInteger minPoolSize;

  BigInteger minPledge;

  BigInteger maxPledge;

  Double minSaturation;

  Double maxSaturation;

  Integer minBlockLifetime;

  Integer maxBlockLifetime;

  Double minVotingPower;

  Double maxVotingPower;

  Double minGovParticipationRate;

  Double maxGovParticipationRate;

  Boolean isShowRetired;
}
