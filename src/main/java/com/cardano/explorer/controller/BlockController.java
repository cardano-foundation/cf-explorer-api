package com.cardano.explorer.controller;

import com.cardano.explorer.config.LogMessage;
import com.cardano.explorer.model.BaseFilterResponse;
import com.cardano.explorer.model.BlockFilterResponse;
import com.cardano.explorer.model.BlockResponse;
import com.cardano.explorer.service.BlockService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import lombok.RequiredArgsConstructor;
import org.springdoc.api.annotations.ParameterObject;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.web.PageableDefault;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/v1/block")
@RequiredArgsConstructor
public class BlockController {

  private final BlockService blockService;

  @GetMapping("/{no}")
  @LogMessage
  @Operation(summary = "Get a block detail by its no")
  public BlockResponse getBlockDetail(
      @PathVariable @Parameter(description="Block number") Integer no) {
    return blockService.getBlockDetail(no);
  }

  @GetMapping
  @LogMessage
  @Operation(summary = "Get blocks by epoch no")
  public BaseFilterResponse<BlockFilterResponse> getBlockByEpochNo(
      @RequestParam @Parameter(description="Epoch number") Integer epochNo,
      @ParameterObject @PageableDefault(sort = {"id"}, direction = Sort.Direction.DESC)
      Pageable pageable) {
    return blockService.getBlockByEpochNo(epochNo, pageable);
  }

//  @Cacheable(value = "block_page", key = "#pageable.pageNumber+''+#pageable.pageSize+''+#pageable.sort")
  @GetMapping("/list")
  @LogMessage
  @Operation(summary = "Get all block")
  public BaseFilterResponse<BlockFilterResponse> getAllBlock(@ParameterObject
      @PageableDefault(sort = {"id"}, direction = Sort.Direction.DESC) Pageable pageable) {
    return blockService.getAllBlock(pageable);
  }

}
