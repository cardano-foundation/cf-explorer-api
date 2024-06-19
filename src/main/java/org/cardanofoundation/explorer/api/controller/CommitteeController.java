package org.cardanofoundation.explorer.api.controller;

import jakarta.validation.Valid;

import lombok.RequiredArgsConstructor;

import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.cardanofoundation.explorer.api.service.GovernanceActionService;
import org.springdoc.core.annotations.ParameterObject;

import org.cardanofoundation.explorer.api.config.LogMessage;
import org.cardanofoundation.explorer.api.model.response.BaseFilterResponse;
import org.cardanofoundation.explorer.api.model.response.committee.CommitteeMemberResponse;
import org.cardanofoundation.explorer.api.model.response.committee.CommitteeOverviewResponse;
import org.cardanofoundation.explorer.api.service.CCommitteeService;
import org.cardanofoundation.explorer.common.entity.ledgersync.CommitteeMember_;
import org.cardanofoundation.explorer.common.validation.pagination.Pagination;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationDefault;
import org.cardanofoundation.explorer.common.validation.pagination.PaginationValid;

@RestController
@RequestMapping("/api/v1/committee")
@RequiredArgsConstructor
@Validated
@Tag(name = "committee", description = "The committee APIs")
public class CommitteeController {

  private final CCommitteeService cCommitteeService;
  private final GovernanceActionService governanceActionService;

  @GetMapping("/overview")
  @LogMessage
  @Operation(
      summary = "Current constitutional committee overview",
      tags = {"committee"})
  public ResponseEntity<CommitteeOverviewResponse> getCommitteeOverview() {
    return ResponseEntity.ok(cCommitteeService.getCommitteeOverview());
  }

  @GetMapping("/members")
  @LogMessage
  @Operation(
      summary = "Get committee members",
      tags = {"committee"})
  public ResponseEntity<BaseFilterResponse<CommitteeMemberResponse>> getCommitteeMembers(
      @ParameterObject
          @Valid
          @PaginationValid
          @PaginationDefault(sort = CommitteeMember_.EXPIRED_EPOCH, direction = Sort.Direction.DESC)
          Pagination pagination) {
    return ResponseEntity.ok(cCommitteeService.getCommitteeMembers(pagination.toPageable()));
  }

  @GetMapping("{publicKey}")
  @LogMessage
  @Operation(
      summary = "Get committee members",
      tags = {"committee"})
  public ResponseEntity<CommitteeMemberResponse> getCommitteeDetail(
      @PathVariable String publicKey) {
    return ResponseEntity.ok(cCommitteeService.getCommitteeMemberDetail(publicKey));
  }
}
