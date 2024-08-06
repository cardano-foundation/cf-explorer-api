package org.cardanofoundation.explorer.api.mapper;

import java.math.BigInteger;
import java.util.Date;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import org.cardanofoundation.explorer.api.model.response.governanceAction.VotingOnGovActionResponse;
import org.cardanofoundation.explorer.api.projection.LatestVotingProcedureProjection;

@Mapper(componentModel = "spring")
public interface LatestVotingProcedureMapper {

  @Mapping(
      target = "timestamp",
      expression = "java(fromLong(latestVotingProcedureProjection.getBlockTime()))")
  @Mapping(target = "isRepeatVote", source = "repeatVote")
  VotingOnGovActionResponse fromLatestVotingProcedureProjection(
      LatestVotingProcedureProjection latestVotingProcedureProjection);

  default Date fromLong(BigInteger value) {
    return value == null ? null : new Date(value.longValue() * 1000);
  }
}
