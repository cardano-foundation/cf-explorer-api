package org.cardanofoundation.explorer.api.mapper;

import java.util.Date;

import com.fasterxml.jackson.databind.JsonNode;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;

import org.cardanofoundation.explorer.api.common.enumeration.VoteType;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionDetails;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionDetailsResponse;
import org.cardanofoundation.explorer.api.model.response.governanceAction.GovernanceActionResponse;
import org.cardanofoundation.explorer.api.projection.GovActionDetailsProjection;
import org.cardanofoundation.explorer.api.projection.GovernanceActionProjection;

@Mapper(componentModel = "spring")
public interface GovernanceActionMapper {
  GovernanceActionResponse fromGovernanceActionProjection(
      GovernanceActionProjection governanceActionProjection);

  @Mapping(source = "type", target = "govActionType")
  @Mapping(target = "voteType", expression = "java(getVoteType(govActionDetailsProjection))")
  @Mapping(
      target = "submissionDate",
      expression = "java(fromLong(govActionDetailsProjection.getBlockTime()))")
  GovernanceActionDetails fromGovActionDetailsProjection(
      GovActionDetailsProjection govActionDetailsProjection);

  GovernanceActionDetailsResponse fromGovActionDetailsWithHistoryVotes(
      GovernanceActionDetails governanceActionDetails);

  default VoteType getVoteType(GovActionDetailsProjection govActionDetailsProjection) {
    if (govActionDetailsProjection.getVote() == null) {
      return VoteType.NONE;
    } else {
      return VoteType.valueOf(govActionDetailsProjection.getVote().name());
    }
  }

  default String fromJsonNode(JsonNode jsonNode) {
    return jsonNode.textValue();
  }

  default Date fromLong(Long value) {
    return value == null ? null : new Date(value * 1000);
  }
}
