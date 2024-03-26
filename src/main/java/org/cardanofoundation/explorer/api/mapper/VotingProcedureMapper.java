package org.cardanofoundation.explorer.api.mapper;

import java.util.Date;

import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import org.cardanofoundation.explorer.api.model.response.governanceAction.HistoryVote;
import org.cardanofoundation.explorer.api.projection.VotingProcedureProjection;

@Mapper(componentModel = "spring")
public interface VotingProcedureMapper {

  @Mapping(source = "votingProcedureTxHash", target = "txHash")
  @Mapping(source = "votingProcedureTxIndex", target = "txIndex")
  @Mapping(
      target = "timestamp",
      expression = "java(fromLong(votingProcedureProjection.getBlockTime()))")
  HistoryVote fromVotingProcedureProjection(VotingProcedureProjection votingProcedureProjection);

  default Date fromLong(Long value) {
    return value == null ? null : new Date(value * 1000);
  }
}
