package com.cardano.explorer.controller;

import com.cardano.explorer.config.LogMessage;
import com.cardano.explorer.model.response.BaseFilterResponse;
import com.cardano.explorer.model.response.BlockFilterResponse;
import com.cardano.explorer.model.response.EpochResponse;
import com.cardano.explorer.model.response.dashboard.EpochSummary;
import com.cardano.explorer.service.BlockService;
import com.cardano.explorer.service.EpochService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import lombok.RequiredArgsConstructor;
import org.springdoc.api.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/epochs")
@RequiredArgsConstructor
public class EpochController {

  private final EpochService epochService;

  private final BlockService blockService;

  @GetMapping("/{no}")
  @LogMessage
  @Operation(summary = "Get a epoch detail by its no")
  public ResponseEntity<EpochResponse> getEpochDetail(
      @PathVariable @Parameter(description = "Epoch Number") String no) {
    return ResponseEntity.ok(epochService.getEpochDetail(no));
  }

  @GetMapping("/{no}/blocks")
  @LogMessage
  @Operation(summary = "Get block list of epoch by its no")
  public ResponseEntity<BaseFilterResponse<BlockFilterResponse>> getBlockList(
      @PathVariable @Parameter(description = "Epoch Number") String no,
      @ParameterObject @PageableDefault(sort = {"id"},
          direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(blockService.getBlockByEpoch(no, pageable));
  }

  @GetMapping
  @LogMessage
  @Operation(summary = "Get all epoch")
  public ResponseEntity<BaseFilterResponse<EpochResponse>> filter(
      @ParameterObject @PageableDefault(sort = {"id"},
          direction = Sort.Direction.DESC) Pageable pageable) {
    return ResponseEntity.ok(epochService.getAllEpoch(pageable));
  }
  @GetMapping("/current")
  @LogMessage
  @Operation(summary = "Get current epoch")
  public ResponseEntity<EpochSummary> findCurrentEpoch(){
    return ResponseEntity.ok(epochService.getCurrentEpochSummary());
  }

}
